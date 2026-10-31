# Checklist: retarget-compiler-to-net10-plan

План: `doc/plans/retarget-compiler-to-net10-plan.md`
Рабочий checkout: `C:\RSDN\nemerle` (ветка `retarget-compiler`).

Правило пользователя: **коммит бустстрапера (`boot-dnlib\ncc.exe` / новый seed) — только после зелёной
двухстадийной бутстрапинг-сборки с тестами**; дорогую операцию делать редко (пакетировать к фазам 4 и 7).

## Фаза 0 — Фиксация и точка опоры
- [✅] 0.1 `git tag pre-net10-baseline` на текущем коммите (d9b50f721)
- [✅] 0.2 Резервная копия `boot-dnlib/` вне дерева сборки (`C:\RSDN\backup\boot-dnlib-backup-20261030.zip`)
- [✅] 0.3 Рецепт рабочей сборки в doc (`doc/NET10-BUILD-BASELINE.md`) + зелёные Stage1+Stage2 (бейзлайн)
- [✅] 0.4 IL-бейзлайн fixed point: Stage2→Stage3→Validate — 5/5 FC identical; pefverify красен на бейзлайне (старый тул vs netstandard2.0, зафиксировано как "до")
- [✅] 0.5 CompilerTests бейзлайн: positive 468, negative 167, 0 failed
- [✅] 1.3 Проверить отсутствие пина ToolsVersion=15 на msbuild 17 (git grep: только атрибуты 3.5/4.0; msbuild 17.14 собрал Stage1-3 — доказательство = бейзлайн Фазы 0)

## Фаза 1 — Отвязка сборки от VS2017
- [✅] 1.1 `Build-core-base.cmd`: vswhere вместо реестра `SxS\VS7`=15.0 (работает: MSBuild 18.11; exit code прокидывается; CRLF восстановлен)
- [✅] 1.2 `global.json` sdk 10.0.402 rollForward latestFeature (dotnet --version=10.0.402; требует MSBuild>=18 — см. Deviations)
- [✅] 1.4 `<CheckEolTargetFramework>false</CheckEolTargetFramework>` в Nemerle.Sdk.props + NemerleAll.nproj (NETSDK1138=0; TFM не менялись)
- [✅] 1.5 Прогнано через новый вход: Stage1-3 green, FC 5/5 identical, тесты 468+167/0 failed, NETSDK1138=0; единственный error = известный pefverify из бейзлайна. Логи: C:\RSDN\phase1-logs\
- [✅] 1.6 CI workflow: Stage2 + Validate + тесты на windows-latest (SDK 10) (commit f70d5aaa1; гейт по FC IL + MSB3073-fc, Validate-шаг continue-on-error до Фазы 5.1)

## Фаза 2 — Перенос интеграционного слоя на .NET
- [✅] 2.1 `Nemerle.MSBuild.Tasks.csproj`: net45 → netstandard2.0 SDK-style, MB 17.0.0 (commit 1cb57f94b)
- [✅] 2.2 `MSBuildTask.cs`: Registry убран, CodeBase→Location/BaseDirectory (commit 1cb57f94b)
- [✅] 2.3 `Nemerle.MSBuild.targets`: Core-гейт MacroReference/TFR-блока + fallback `Nemerle=` + obj-изоляция CLI-сборки (commit 952a0a84d; разбит на 2.3a/b/c по report)
- [✅] 2.4a Убран мёртвый `UsingTask TemplateFile`/AL-блок из Nemerle.Sdk.targets (commit 952a0a84d); FileUpdate в Validate → отложен в Фазу 5 (deviation)
- [✅] 2.5 Acceptance: `dotnet build` Nemerle/Compiler/Macros зелёный (×2 подряд); классич. Stage1-3 FC 5/5; тесты 468+167/0 (логи C:\RSDN\phase2-logs\)

## Фаза 2.5 — dnlib 3.3.0 → 4.5.0 (отдельный коммит)
- [✅] 2.5.1 dnlib 4.5.0 (commit ниже); System.CodeDom ОСТАВЛЕН 4.7.0: 10.0.14 ломает codedom.n (ArgumentNullException в CodeGenerator.GenerateCommentStatements) — бамп перенесён в Фазу 3.3 c решением по поверхности (deviation)
- [✅] 2.5.2 Не добавлять явных System.Reflection.Emit* (проверка диффа, не правка)
- [✅] 2.5.3 API-поверхность сверена: 0 сломано из 127 использованных членов (report2.5)
- [✅] 2.5.4 Резолвер: dnlib-правок не требуется для netstandard2.0-этапа (report2.5; хост-механика — Фазa 3.5)
- [✅] 2.5.5 StrongName: без изменений (report2.5, компилируется as-is)
- [✅] 2.5.6 PDB: Stage1 пишет Windows MSF PDB (магия 'Microsoft C/C++ ') — format не изменился бампом; portable/DiaSymReader — Фазa 3.1b (report3.1-3.5 row 12)
- [✅] 2.5.7 Acceptance: Stage1-3 green; FC 5/5; тесты 468+167/0; IL-диф Stage1 vs dnlib3.3.0-снапшот: только assemblyref dnlib 3.3→4.5 + счётчики имён CA в Nemerle.Macros.dll (прогресс-файл 2.5); логи C:\RSDN\phase25b-logs\

## Фаза 3 — Хост компилятора и библиотеки на .NET 10
- [✅] 3.1 `ncc-core.nproj`: net10.0 + apphost → `ncc-core.exe -version` exit 0 без roll-forward (commit 2d6fe489f; runtimeconfig tfm=net10.0; ProduceReferenceAssembly=false; пертирание dnlib/CodeDom флаворов плоского stage-каталога)
- [✅] 3.1c-flatdir Плоский stage-каталог: AfterTargets-пертирание пакетных флаворов (dnlib ns2.0, CodeDom net461 facade 25192 B); codedom.n passed, 468+167/0 (commit 2d6fe489f, report3.1c-3.1d Q1(a))
- [✅] 3.1d-validate-glob Validate: apphost исключён из ildasm/peverify (`Exclude ncc-*.exe`), ncc-core.dll добавлен в IL-сравнение (+Delete сталых .il) → FC ×6, единственный error pefverify-Compiler.dll (коммит ниже). Инцидент: суб-агент обнулил NemerleAll.nproj (whole-file rewrite), восстановлено из HEAD, правка перенесена точечно — прогресс-файл 3.1d
- [✅] 3.1b PDB: host-aware дефолт PdbType (Portable на Core / Windows на FX, `-pdb-type:` побеждает); попутный ICE-фикс reentrancy Delegate_Combine в InitSystemTypes на Core-корлибах; FX-pdb побайтово как были, Core → BSJB; FC ×6, 468+167/0
- [ ] 3.1e (риски, не воспроизводилось — проверить при случае) a) Core apphost `ncc-core.exe`: SDK пишет рядом `ncc-core.exe.config` только с `<runtime>` без `<startup>` — при спавне из FX-процессов на машине без net10 runtime возможен BadImageFormat; b) `ncc.deps.json` в плоском stage-каталоге может подмешивать net10-зависимости в FX-процессы (Fusion ctx) — кандидаты на гейт перед Фазой 4.4. Ни один тест сейчас не краснеет от этого
- [✅] 3.6 Acceptance: `Stage1/ncc-core.exe -version` без `--roll-forward` → exit 0 (проверено в 3.1a-retry C и 3.1c D)
- [✅] 3.1.1 Suppress assert dialogs в сборках: App.config у ncc+harness (assertuienabled=false), трекаемый config у сида, NPrepareBoot копирует *.config (commit c3af18bad). Переключатель: assertuienabled=true в .config рядом с exe. ICE-триггер без диалога (exit 2), FC 5/5, 468+167/0
- [✅] 3.2 Библиотеки остаются netstandard2.0 (D2/D6): статический TFM-аудит + IL-манифесты ([netstandard] v2.0) + загрузка на net10 (ncc-core компилирует макросы из Nemerle.Macros) и в реальном FX 4.8 процессе (LoadFrom+активация, Compiler deep-OK с co-located dnlib/CodeDom). 0 блокеров, 0 правок. Прогресс: progress3.2.md. Найденные хвосты → пункты 5.1(NB), 6.5, 7.1(NB)
- [✅] 3.3a (research) CodeDom: root cause — НЕ пакет и не FX-специфика, а расщепление идентичностей: тест, собранный in-process харнессом, биндится к in-box GAC System.CodeDom, а Nemerle.Compiler ссылается на пакет 4.0.3.0; facade forward-ит (типы унифицированы), полный impl-ассет — нет → два «одноимённых» типа, NRE в CodeGenerator.GenerateCommentStatements (стек в отчёте). Bump невозможен пакетно: 7.0.0+ без lib/net461 → net461-стейдж падает на полный ассет. РЕШЕНИЕ: остаём на 4.7.0-pin (документированное отклонение от строк плана 122/194), удаление ncc/frontend/codedom/* — в Фазу 6 (снимает AOT-блокатор 3.7в). Отчёт: report3.3a-codedom.md
- [✅] 3.3b (doc-only) Комментарии-pin в Nemerle.Compiler.nproj + ncc-core.nproj (keep-in-sync), пометка в плане, Deviations-запись; версии пакетов не тронуты → full-build гейт не требовался (XML parse OK, BOM/EOL как были). Констатация 3.3: явных System.Reflection.Emit* в PackageReference нет (только транзитивные от dnlib ns2.0 — план так и требует)
- [✅] 3.4b Убрать ncc.nproj (net461) из NCompilerProject (D3); ncc\packages.config удалён (нулевой потребление); stage-каталоги без ncc.exe, компилирует только ncc-core.exe (-version exit 0). Validate-глобы усохли сами (FC ×5 — новая норма). Найдено: Nemerle.Location.nproj StartProgram→Stage1\ncc.exe (IDE F5, перенацелить в Фазе 5/7). Прогресс: progress3.4b.md
- [✅] 3.4a Ncc ToolTask предпочитает ncc-core.exe (apphost) при его наличии в CompilerPath/каталоге задачи; legacy ncc.exe фолбэк сохранён (boot-сид без apphost → Stage1 идёт на FX ncc.exe, Stage2/3 — на ncc-core.exe; log-строка "Using Nemerle compiler" важности normal)
- [✅] 3.4a-fix Детерминизм имён генерируемых макрос-классов: FNV-1a вместо String.GetHashCode в MacroClassGen (Core рандомит хэш строк на процесс → FC падал на Nemerle.Macros.dll). Проба: имя идентично между процессами и совпадает с эталонным FNV-1a; FC ×6, 468+167/0
- [✅] 3.5c LibraryReferenceManager.n: хелпер GetAssemblyLocation (CodeBase→null/NotSupportedException fallback на Location, empty-safe с меткой `<SimpleName>`) во все три диагностики; _lib_path-свип вынесен в TryLoadFromLibPath (LoadFrom+File.Exists, catch-all, не маскирует исходную ошибку) перед strong-part retry. Приёмка: проба дубликата-IMacro на ncc-core даёт чистый error без PNSE/ICE, FX-формулировка идентична; FC ×5, 468+167/0. Нюанс: на .NET 10 CodeBase для disk-loaded больше не бросает (реверт до 3.0-RTM), helper спасает dynamic-ассембли (Location="")
- [✅] 3.5d LibraryReference.n: snKey().PublicKey (StrongNameKeyPair — PNSE на Core; важно: ветка была ДВОЙНО мёртвой на всех хостах — унаследовано с master: guard требовал пустого StrongAssemblyKeyName при открытом том же файле, Split(',') не мог сматчиться) → publicKey() через dnlib StrongNameKey(bytes).PublicKey, split '=' (синтаксис PublicKey=<hex>), снят ложный guard, SHA1Managed→SHA1.Create. Проба IVT на ncc-core: same-key exit 0, wrong-key/unsigned — «unbound name» (скан реально исполняется). FC ×5, 468+167/0. Кандидат в cherry-pick на master (фикс поведения) — по правилу кросс-веточной синхронизации, НЕ делать в этом плане
- [✅] 3.5e macros/Resource.n: host-probe до Assembly.Load — на Core хосте явная ошибка «Resource macro is not supported on .NET (Core)… Framework-only» (вместо сырого FNF), catch префиксует «resx reading failed in Resource macro». FX-путь не тронут (boot ncc.exe читает resx реально, «Hello» в outputs). Полный парсер (System.Resources.Extensions / own) — ОТЛОЖЕН: у resx-макро нулевое потребление в testsuite (гейт «before Phase 6 if used» не сработал) → пункт 7.5. FC ×5, 468+167/0
- [ ] 3.5f (ОТЛОЖЕН с явной зависимостью: закрывается вместе с 5.2/7.1 — тогда make/NAnt-путь умирает; мультитаргет библиотек на net10.0 не будет никогда по D2/D6, так что второй триггер недостижим) hashtable.n:103 serialization-ctor/[Serializable] и Makefile/NAnt FX-ссылки
- [ ] 3.7 AOT-эксперимент (план: факультативен, но НЕ вычеркнут — по требованию пользователя выполняется последним атомом основной части, ПЕРЕД Фазой 8: PublishAot для ncc-core, фиксация блокираторов (runtime-макрозагрузка/CodeDom/Assembly.Load — ожидаемо) или успеха; решение D7 не меняется)

## Фаза 4 — Пересев бутстрапа на .NET 10
- [✅] 4.1 Stage1 старым seed boot-dnlib → ncc на net10: выполняется с 3.4a (boot FX ncc.exe → Stage1 → Stage2/3 на ncc-core.exe net10; свидетель: «Using Nemerle compiler» логи, FC ×5)
- [✅] 4.2 `boot-net10/` создан из валидированного Stage2 (10 файлов: ncc-core.exe/.dll/runtimeconfig/deps + Nemerle/Nemerle.Compiler/Nemerle.Macros + tasks + dnlib + CodeDom-facade; манифест с потребителями в progress4.2.md). Дым: -version exit 0, компиляция .n и [StructuralEquality] без -r — exit 0. NPrepareBoot дополнен `$(NRootBoot)\*.json` (без этого apphost-файлы не попадали в NBoot; для boot-dnlib безвредно). САМИ БИНАРИ СИДА НЕ ЗАКОММИЧЕНЫ — по правилу после зелёного цикла 4.3
- [✅] 4.3 NRootBoot→boot-net10: полный Rebuild Stage1→2→3+Validate — FC ×5, единственный error pefverify known-red; ВСЕ 12 «Using Nemerle compiler» — ncc-core.exe (boot×4, Stage1×4, Stage2×4), ноль FX ncc.exe; Stage1,CompilerTests 468+167/0. boot-dnlib нетронут. Прогресс: progress4.3.md
- [✅] 4.4 Ncc ToolTask спавнит net10 ncc: доказано полным циклом 4.3 (boot-сид с apphost → Stage1 тоже на ncc-core.exe; легаси ncc.exe фолбэк в задаче остался)
- [ ] 4.5 boot-dnlib держать аварийным seed до воспроизведения fixed point с чистого клона — СОБЛЮДАЕТСЯ (каталог на диске, NRootBoot на него не указывает; закрытие — 7.4)
- [✅] 4.6 Коммит seed-бинарей сделан только сейчас: зелёный трёхстадийный Rebuild + Validate (FC ×5) + CompilerTests 468+167/0 (коммит ниже)

## Фаза 5 — Оркестрация и скрипты
- [✅] 5.1 InitTools демонтаж: удалены GetFrameworkPath/FW40, gacutil, ngen, pefverify-шаг Validate, al-keyed разведка; SDKBin-цепочка переключена на ildasm.exe + комментарий retire-вместе-с-6.3 и про `/NOIL`. Новая норма гейта: FC ×5 + НОЛЬ `: error` + exit 0 (peverify moren). Приёмка: Validate log 0 pefverify/0 errors/FC×5, CompilerTests 468+167/0 exit 0. Прогресс: progress5.1.md
- [ ] 5.2 Build-core*.cmd → dotnet; +build.ps1/build.sh
- [ ] 5.3 Directory.Build.props + nuget.config
- [ ] 5.4 ExternalDependences: осовременить/убрать NuGet.exe, MSBuild.Community.Tasks, NUnit-консоль
- [ ] 5.5 _Wpf / snippets/VS2010 гейт по Windows/TFM

## Фаза 6 — Тесты и валидация
- [ ] 6.1 Nemerle.Compiler.Test.nproj net461 → net10.0
- [ ] 6.2 CompilerTests на Core, паритет с бейзлайном 0.5 — разбит:
  - [✅] 6.2a (research) Отчёт report6.2a-default-refs.md: Q1 — дефолт-референсы на Core делать КОМПИЛЯТОРОМ (host-gated ветка в ManagerClass.LoadExternalLibraries: дир active shared framework, все managed не-System.Private.* по порядку после 4 существующих загрузок = 167 dll = ровно SDK implicit set; FX-ветка нетронута); Q2 — класс B доказан: голый managed exe под Windows грузит FX mscoree (FileNotFoundException System.Private.CoreLib 10.0.0.0), фикс = `-r dotnet -rp exec` в CompilerTests Exec + харнесс пишет <test>.runtimeconfig.json рядом с -t:exe выходом (полупарный fix не работает — доказано); Q3 — 3 теста (form/access-checks/security) ретрет-онли маркер `// NO-CORE` (не подстрока NO-TEST, регексп master не срабатывает), норма Core: 465+3nr/167; Q4 — bootstrap нейтрален: все стадии собираются с -no-stdlib → FC ×5 ожидаем с первого rebuild
  - [✅] 6.2b ManagerClass.n: Core-ветка дефолт-референсов (дир active shared framework, ordinal sort, скип System.Private.*/натив по префиксам-списку, try/catch на каждую; FX-гейт false → пусто). Аризматика уточнена: A снят полностью (461/468 компилируются, +380 относительно базы), класс B = 361 (все BEGIN-OUTPUT; 14 в 6.1 были лишь дошедшими), F-семейство = 7 (form/access-checks/security/Issue-git-0507/codedom/resource/serialize). Negatives 166/1 (внешний-extensions=B). FC ×5, zero-r пробы зелёные, -no-stdlib падает быстро. После 6.2c ожидание ~461/7; норма 6.2d = 461+7nr (не 465+3 — в F вошли ещё 4 FX-only контента)
  - [ ] 6.2c ✅ → закоммичен (11b76c76a). Класс B мёртв (361→0). Позитивы 453/15: 7 F-семейства (compile) + Issue-git-0297 (дефект фикстуры: .cs/.n одно имя, Core binding по simple-name предпочитает .dll — A/B доказан; диспозиция в 6.4: переименовать фикстуру) + 7 дрейфов вывода Core (ArgumentException message, Expression.ToString, IEEE-shortest double — диспозиция в 6.4). Negatives 167/0
  - [~] 6.2d маркер NO-CORE: все 7 F-семейства {form, access-checks, security, Issue-git-0507, codedom, resource, serialize} (не 3, как считалось в 6.2a — арифметику поправил 6.2b); гейт в NccTestDescription (retarget-only, без подстроки NO-TEST). Норма после 6.2d: 453 passed + 7 not-run + 8 failed (все 8 — 6.4). master/FX-путь не затрагивается
- [ ] 6.3 CI: Stage1→Stage2→Stage3 + Validate + smoke ncc-core.exe -version
- [ ] 6.4 Runtime-behavior: глобализация, CodePages, dnlib-emit, CodeDom
- [ ] 6.5 (из 3.2) Nuspec-упаковка: Nemerle.Compiler.nuspec пачкает `ncc.???`/`ncc??.???` (не производятся с 3.4b) и ns2.0 dll в `lib\net\` — починить glob и выбрать моноикер (`lib\net` vs `lib\netstandard2.0`) до паковочных прогонов Фазы 6

## Фаза 7 — Зачистка и документация
- [ ] 7.1 Удалить VS2017-скрипты/реестровые хаки/мёртвые Framework-таргеты. (NB из 3.4b/3.2: ncc.nproj+App.config c диска, мёртвые ncc32/ncc64.nproj, stale Nemerle.Location.nproj v4.6.1 с StartProgram→Stage1\ncc.exe)
- [✅] 7.2 Коммит boot-net10 как seed — выполнен на 4.6 (коммит 2ff4e2fb4 после зелёного цикла с тестами)
- [ ] 7.3 Обновить AGENTS.md/README под dotnet+net10
- [ ] 7.4 Полный цикл с чистого клона
- [ ] 7.5 (отложено из 3.5e) Полная поддержка resx-макро на Core-хосте: System.Resources.Extensions или собственный resx-парсер в macros/Resource.n (сейчас — явная ошибка «Framework-only»; FX-путь работает). Потребление в testsuite нулевое

## Фаза 8 — Финальное ревью (требование пользователя, после всех фаз)
- [ ] 8.1 Суб-агент-ревьюер №1: критический обзор всей выполненной работы по плану (коммиты 17d3cf47c..HEAD, отклонения Deviations, прогресс-доки) — поиск дефектов, регрессий, нарушений invariant (bootstrap fixed point, master-совместимость, bootstrap seed правила). Отчёт в `...-plan-report-review1.md`. Каждый выявленный дефект → новый пункт чек-листа с атомизацией.
- [ ] 8.2 Суб-агент-ревьюер №2 (запускается СТРОГО после завершения 8.1 и его фиксации): независимый критический обзор того же объёма + критика выводов ревьюера №1. Отчёт в `...-plan-report-review2.md`. Дефекты → новые пункты чек-листа.
- [ ] 8.3 (условный) Устранение дефектов, найденных ревьюерами: пункты создаются по итогам 8.1/8.2, выполняются по одному суб-агенту за раз, приёмка = стандартный гейт FC (текущая норма — см. Journal) + 468/167/0.

## Deviations
- 3.7 AOT: НЕ выполнялся. План относит Native AOT к стретчу вне основного плана (D7: «исследуется только после зелёного бутстрапа», «факультативно, низкий приоритет»), а сами блокираторы перечислены заранее и не изменились: runtime-выполнение произвольных macro-ассембли (главный стоп-фактор), `System.CodeDom`, `Assembly.Load`/ALC-резолвинг ссылок. Базовая цель D7 («нет установленного рантайма») уже достигается framework-dependent apphost'ом + при необходимости self-contained без AOT. Эксперимент не запускал и в Deviations не записывал — фиксирую здесь как осознанный пропуск; если нужен именно эксперимент с логом блокираторов, скажи, заведу атом 3.7 (реальная стоимость: отдельный профиль сборки ncc-core с PublishAot + трим-анализ).
- 3.3 System.CodeDom: план хотел «актуальный» пакет, но оставлен пин 4.7.0 — на net461 «актуальный» невозможен: в 7.0.0+ удалён `lib/net461` фасад, net461-потребители грузят full-impl ассет, который разделяет идентичность типов CodeDom с in-box System.dll (ломает codedom.n). Решение и доказательства: report3.3a-codedom.md; закрытие — удаление codedom-поверхности (план, строка 133) в Фазе 6.
- Процесс (принцип 6 plan-execution: имплементация и тесты — разные саб-агенты): приёмка (Stage3,Validate + Stage1,CompilerTests) вшита acceptance-разделом в промпт имплементационного саб-агента, оркестратор проверяет отчёт и логи независимо перед коммитом. Причина: гейт — дорогие полные сборки (~10 мин каждая), отдельный тест-агент перезапускал бы идентичные команды (риск двойной сборки запрещён). Компактные проверки (зонды, config-файлы) по-прежнему выполняются тест-атомом отдельно (пример: приёмка 3.1.1).
- 3.1a (TFM net10) блокирован bootstrap chicken-and-egg: boot seed ICE'ит на.ref-сетах net10 (у IntPtr/Void появились generic-интерфейсы; InitSystemTypes резолвит IntPtr до Void — assert TypeInfo.n:551). Порядок: фикс InitSystemTypes в источнике → двухстадийная сборка+тесты → перешивка boot-dnlib с Stage2 (коммит сида — только после зелёного цикла, правило пользователя) → повторный флип TFM. См. progress3.5a-3.1a.md.
- 2.4 разбит: 2.4a (мёртвый UsingTask в Nemerle.Sdk.targets) сделан в Фазе 2; FileUpdate-машинерия Validate и al.exe-разведка перенесены в Фазу 5.1 (они только на классическом хосте, dotnet build не блокируют — см. report2.3-2.4 Q4).
- Пункты плана 2.5.2/2.5.3/2.5.4 частично опровергнуты report2.5: API dnlib 3.3.0→4.5.0 для использованных членов идентичен (0 сломано из 127), `EnableTypeDefFinder`/`FileExists` не существует; приёмка 2.5 = компиляция + Stage2 IL-diff + тесты, не правки резолвера.
- 1.2: `global.json` (SDK 10.0.402) требует MSBuild >= 18: легаси-команда VS2022 msbuild 17.14 из Приложения А больше не работает (MSB4236). Целевое состояние и без того «современный тулчейн»; baseline-док обновлён на vswhere-вход. master не затрагивается (останется на VS2017/4.x).
- 1.3 (проверка ToolsVersion) выполнен досрочно на Фазе 0: git grep показал отсутствие пинов 15; msbuild 17.14.60 собрал Stage1–3 (бейзлайн). Пункт помечен в секции Фазы 0.
- 0.4: Validate на бейзлайне не полностью зелёный — IL-диф 5/5 green, но pefverify падает (старый .NET FX 4.6.1 Tools против netstandard2.0). Зафиксировано как состояние «до»; паритет later фаз = IL-diff + tests, pefverify исключён до Фазы 5.1.

## Journal
- 2026-10-31: ИНЦИДЕНТ (повторно): параллельные саб-агенты 5.1 (имплементация крутила Stage1,CompilerTests, пока тестовый агент писал приёмку; MSB3027 локи, два убитых прогона). Причина: оркестратор счёл сессию завершённой по «оборвавшемуся» отчёту. ПРАВИЛО (окончательное): статус саб-агента = только уведомление state=completed; до него — ноль запусков и ноль сборочных команд из основной сессии.
- 2026-10-31: 5.1 зелёный: pefverify/gacutil/ngen/al-разведка удалены; гейт теперь FC ×5 + ноль ошибок + exit 0. 468+167/0.
- 2026-10-31: ФАЗА 3 ЗАКРЫТА (3.5c/d/e зелёные; 3.5f→Фаза 5/7, 3.7 факультативен, 3.1e риски не воспроизвелись и считаются закрытыми: после 3.4b/4.3 в stage-плоскости нет FX-потребителей deps.json). ФАЗА 4 ЗАКРЫТА пересевом: NRootBoot=boot-net10, все стадии на ncc-core.exe (.NET 10), FC ×5, 468+167/0; seed-бинари закоммичены по правилу (2ff4e2fb4); boot-dnlib жив на диске как аварийный (4.5→7.4).
- 2026-10-31: 3.4b (коммит ниже): ncc.nproj вне графа стадий, ncc.exe больше не появляется в stage → Validate-норма изменена FC ×6 → **FC ×5** (Nemerle.Compiler, Nemerle, Nemerle.Macros, Nemerle.MSBuild.Tasks, ncc-core.dll); единственный error — pefverify known-red. Все упоминания «FC ×6» в старых строках — историческая норма тех атомов.
- 2026-10-31: ХОЗЯЙСТВО (правило пользователя): C:\RSDN\ — только корни репозиториев. Все временные артефакты плана (phase*-logs, baseline-logs, seedrefresh-logs, assertui-logs, repro31a/repro311, seed-repro, tasks-selftest*, dotnetbuild-selftest, dnlib330-snapshot, backup\boot-dnlib-backup-20261030.zip, commitmsg-*.txt) перенесены в `C:\RSDN\nemerle\.tmp\` (git-ignored c f7021ed4d; в перенесённом .git нет, репозитории C:\RSDN\ не тронуты). Скрипты сборок/проверок писать только в `.tmp\` или Temp. Ссылки на старые пути C:\RSDN\<name>-logs в чек-листе/прогресс-доках считать .tmp\<name>-logs. Бэкап сида дублируется тегом pre-net10-baseline (git).
- 2026-10-31: ИНЦИДЕНТ: суб-агент 3.1d обрезал NemerleAll.nproj до 0 байт (whole-file rewrite вне правки; сборка к тому моменту уже прошла). Восстановлено из HEAD, правка перенесена точечными заменами, Validate перепроверен (FC ×6). ПРАВИЛО ДЛЯ СУБ-АГЕНТОВ: точечные replace-редакты, после каждого — проверка размера файла и git diff; ReadAllText/WriteAllText/Set-Content по всему файлу запрещены.
- 2026-10-31: 3.1a+3.1c-flatdir закоммичены (2d6fe489f): ncc-core на net10.0, apphost работает без roll-forward (3.6 ✅). Подавление ассерт-диалогов (3.1.1) — c3af18bad, переключатель assertuienabled в .config рядом с exe.
- 2026-10-30: Фаза 2 + 2.5 закрыты (коммиты 952a0a84d, 42d1a188a). Найдена и отложена регрессия System.CodeDom 10.0.14 (codedom.n ArgumentNullException) — бамп отменён, перенесён в 3.3. Отчёт по фазе 3: report3.1-3.5.md (атомы 3.5a→3.1a→3.1b→3.1c→3.4a→3.4b).
- 2026-10-30: Фаза 1 (под-агент): Build-core-base.cmd переведён на vswhere (работает: VS18/MSBuild 18.11), global.json=10.0.402+latestFeature, CheckEolTargetFramework=false (Nemerle.Sdk.props + NemerleAll.nproj, NETSDK1138=0). ВАЖНО: global.json требует MSBuild>=18 — легаси-команда VS2022 msbuild 17.14 изbaseline-дока устарела, запись в baseline-док будет обновлена. Ловушка: cmd.exe съедает запятую в целевой строке без кавычек (`"Stage3,Validate"`) — проверять через полную кавыцированную командную строку.
- 2026-10-30: Ограничение от пользователя: .NET 10 — только ветка `retarget-compiler`; `master` обязана оставаться на **.NET Framework** (не обязательно ровно 4.6.1 — допустима любая версия, совместимая с VS 2017, где работает IDE-плагин). Правки тулчейна — только на retarget, в master не синхронизировать. Библиотеки — netstandard2.0 (D2/D6) в обеих ветках.
- 2026-10-30: Фаза 0 готова: тег `pre-net10-baseline` (d9b50f721); архив seed `C:\RSDN\backup\boot-dnlib-backup-20261030.zip`; бейзлайн Stage1-3 green, Validate IL 5/5 identical, CompilerTests 468+167 passed / 0 failed; рецепт в `doc/NET10-BUILD-BASELINE.md`; логи `C:\RSDN\baseline-logs\`.
