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

struct _MonoProfiler {
	GHashTable *methods;
	MonoMemPool *mempool;
	unsigned long long total_time;
	struct activation_record *the_stack;
	struct activation_record *free_records;
};


static void
output_method (MonoMethod *meth, struct method_descriptor *desc, MonoProfiler *prof)
{
  //        assert (desc->used_times == 0);

	printf("%10lld %10lld %7d  %s %s\n", 
	       (desc->cumulative_time - desc->children_time) / (prof->total_time / 100000),
	       desc->real_cumulative / (prof->total_time / 100000),
	       desc->calls,
	       mono_method_full_name (meth, TRUE),
	       desc->flags & 1 ? "(flawed)" : "");
}

static void
simple_shutdown (MonoProfiler *prof)
{
	g_hash_table_foreach (prof->methods, (GHFunc) output_method, prof);
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

void mono_profiler_startup(const char *args)
{
	MonoProfiler *prof = g_new0 (MonoProfiler, 1);

	(void) args;

	prof->methods = g_hash_table_new (NULL, NULL);
	prof->mempool = mono_mempool_new ();

	mono_profiler_install (prof, simple_shutdown);
	mono_profiler_install_enter_leave (simple_method_enter, simple_method_leave);
	mono_profiler_set_events (MONO_PROFILE_ENTER_LEAVE);
}
