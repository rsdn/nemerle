#include <mono/metadata/profiler.h>
#include <mono/metadata/debug-helpers.h>

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>


typedef union {
	unsigned long long value;
	unsigned int ints[2];
} TSC_value;

#define rdtsc(v) \
        __asm__ __volatile__("rdtsc" : "=a" (v.ints[0]), "=d" (v.ints[1]))

struct method_descriptor {
	MonoMethod *method;
	unsigned int calls;
	unsigned int flags;
	unsigned int used_times;
	unsigned long long cumulative_time;
	unsigned long long real_cumulative;
	unsigned long long children_time;
};

struct activation_record {
	struct activation_record *next;
	struct method_descriptor *method;
	unsigned long long entry_time;
	unsigned long long children_time;
};

struct class_descriptor {
	MonoClass *klass;
	unsigned cumulative_size;
	unsigned instance_count;
};

struct _MonoProfiler {
	GHashTable *methods;
	GHashTable *classes;
	MonoMemPool *mempool;
	unsigned long long total_time;
	unsigned long long total_memory;
	struct activation_record *the_stack;
	struct activation_record *free_records;
};


static void
output_method (MonoMethod *meth, struct method_descriptor *desc, MonoProfiler *prof)
{
  //        assert (desc->used_times == 0);

	printf("%10lld %10lld %10d %8lld %12lld   %s %s\n", 
	       (desc->cumulative_time - desc->children_time) / (prof->total_time / 100000),
	       desc->real_cumulative / (prof->total_time / 100000),
	       desc->calls,
	       (desc->cumulative_time - desc->children_time) / desc->calls,
	       desc->real_cumulative / desc->calls,
	       mono_method_full_name (meth, TRUE),
	       desc->flags & 1 ? "(flawed)" : "");
}

static void
output_class (MonoClass *klass, struct class_descriptor *desc, MonoProfiler *prof)
{
	(void)prof;

	printf("%10u %10u %8.2f  %s\n", desc->cumulative_size, desc->instance_count,
		(double)desc->cumulative_size / desc->instance_count,
		mono_type_full_name (mono_class_get_type (klass)));
}

static void
simple_shutdown (MonoProfiler *prof)
{
	if (1) {
		printf("%10s %10s %10s %8s %12s   %s\n",
			"Total [%]", "Self [%]", "Cnt called",
			"Cyc/call", "TotCyc/Call", "Name");
		g_hash_table_foreach (prof->methods, (GHFunc) output_method, prof);
	}

	if (0) {
		printf("%10s %10s %8s  %s\n",
			"Size", "Instances", "InstSize", "Name");
		g_hash_table_foreach (prof->classes, (GHFunc) output_class, prof);
	
	}

	printf ("managed bytes allocated:\t%llu\n", prof->total_memory);
}

static void
simple_method_enter (MonoProfiler *prof, MonoMethod *method)
{
	struct method_descriptor *desc;
	struct activation_record *record;
	TSC_value tsc_value;

	desc = g_hash_table_lookup (prof->methods, method);
	
	if (desc == NULL) {
		desc = mono_mempool_alloc0 (prof->mempool,
				            sizeof (struct method_descriptor));
		g_hash_table_insert (prof->methods, method, desc);
		desc->method = method;
	}

	desc->calls++;
	desc->used_times++;

	assert (method == desc->method);

	if (prof->free_records == NULL) {
		record = mono_mempool_alloc (prof->mempool, 
					     sizeof (struct activation_record));
	} else {
		record = prof->free_records;
		prof->free_records = record->next;
	}

	record->next = prof->the_stack;
	record->method = desc;
	prof->the_stack = record;
	rdtsc (tsc_value);
	record->entry_time = tsc_value.value;
	record->children_time = 0;
}

static void
simple_method_leave (MonoProfiler *prof, MonoMethod *method)
{
	struct activation_record *record = prof->the_stack;
	struct method_descriptor *desc = record->method;
	unsigned long long running_time;
	TSC_value tsc_value;
	
	while (desc->method != method) {
		desc->flags |= 1;
		desc->used_times--;
		prof->the_stack = record->next;
		record->next = prof->free_records;
		prof->free_records = record;

		record = prof->the_stack;
		if (record == NULL) {
                  printf ("unwind failed for `%s'\n", mono_method_full_name (method, TRUE));
			abort ();
		}
		desc = record->method;

		//printf("expecting %s, got %s\n", desc->method->name, method->name);
	}

	desc->used_times--;

	rdtsc (tsc_value);
	running_time = tsc_value.value - record->entry_time;
	desc->cumulative_time += running_time;
	desc->children_time += record->children_time;
	if (desc->used_times == 0)
		desc->real_cumulative += running_time;
	
	if (record->next)
		record->next->children_time += running_time;
	else
		prof->total_time += running_time;
	
	prof->the_stack = record->next;
	record->next = prof->free_records;
	prof->free_records = record;
}

static void
simple_allocation (MonoProfiler *prof, MonoObject *obj, MonoClass *klass)
{
	struct class_descriptor *desc = 
		g_hash_table_lookup (prof->classes, klass);
	size_t size;

	if (desc == NULL) {
		desc = mono_mempool_alloc0 (prof->mempool,
				            sizeof (struct class_descriptor));
		g_hash_table_insert (prof->classes, klass, desc);
		desc->klass = klass;
	}

	size = mono_object_get_size (obj);
	
	prof->total_memory += size;
	desc->instance_count++;
	desc->cumulative_size += size;
}

void mono_profiler_startup(const char *args)
{
	MonoProfiler *prof = g_new0 (MonoProfiler, 1);

	(void) args;

	prof->methods = g_hash_table_new (NULL, NULL);
	prof->classes = g_hash_table_new (NULL, NULL);
	prof->mempool = mono_mempool_new ();

	mono_profiler_install (prof, simple_shutdown);
	mono_profiler_install_enter_leave (simple_method_enter, simple_method_leave);
	mono_profiler_install_allocation (simple_allocation);
	mono_profiler_set_events (MONO_PROFILE_ENTER_LEAVE | MONO_PROFILE_ALLOCATIONS);
}
