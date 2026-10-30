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
- [✅] 3.6 Acceptance: `Stage1/ncc-core.exe -version` без `--roll-forward` → exit 0 (проверено в 3.1a-retry C и 3.1c D)
- [✅] 3.1.1 Suppress assert dialogs в сборках: App.config у ncc+harness (assertuienabled=false), трекаемый config у сида, NPrepareBoot копирует *.config (commit c3af18bad). Переключатель: assertuienabled=true в .config рядом с exe. ICE-триггер без диалога (exit 2), FC 5/5, 468+167/0
- [ ] 3.2 Библиотеки остаются netstandard2.0 (D2/D6); проверить загрузку из net10 и Framework 4.6.1+
- [ ] 3.3 Пакеты: System.CodeDom актуальный; явных SRE* не добавлять
- [ ] 3.4 Убрать ncc.nproj (net461) из NCompilerProject (D3)
- [ ] 3.5 Починить API-разрывы Core 2.1→10 по таблице (main.n CodeBase, LibraryReferenceManager, ManagerClass ref-set, codedom, Resource.n, hashtable.n и др.)
- [ ] 3.7 (факультативно) AOT-эксперимент; при неудаче — зафиксировать блокиратор

## Фаза 4 — Пересев бутстрапа на .NET 10
- [ ] 4.1 Stage1 старым seed boot-dnlib → ncc на net10
- [ ] 4.2 Каталог `boot-net10/` (framework-dependent, D4)
- [ ] 4.3 NRootBoot→boot-net10: Stage1→Stage2→Stage3→Validate, fixed point IL Stage2≡Stage3
- [ ] 4.4 Ncc ToolTask спавнит net10 ncc (ToolName/CompilerPath)
- [ ] 4.5 boot-dnlib держать аварийным seed до воспроизведения fixed point с чистого клона
- [ ] 4.6 (привязка к правилу пользователя) Коммит seed-бинарей — только после зелёной Stage2 + CompilerTests

## Фаза 5 — Оркестрация и скрипты
- [ ] 5.1 Убрать InitTools Framework-разведку (gacutil/ildasm/al/ngen/peverify) из NemerleAll.nproj
- [ ] 5.2 Build-core*.cmd → dotnet; +build.ps1/build.sh
- [ ] 5.3 Directory.Build.props + nuget.config
- [ ] 5.4 ExternalDependences: осовременить/убрать NuGet.exe, MSBuild.Community.Tasks, NUnit-консоль
- [ ] 5.5 _Wpf / snippets/VS2010 гейт по Windows/TFM

## Фаза 6 — Тесты и валидация
- [ ] 6.1 Nemerle.Compiler.Test.nproj net461 → net10.0
- [ ] 6.2 CompilerTests на Core, паритет с бейзлайном 0.5
- [ ] 6.3 CI: Stage1→Stage2→Stage3 + Validate + smoke ncc-core.exe -version
- [ ] 6.4 Runtime-behavior: глобализация, CodePages, dnlib-emit, CodeDom

## Фаза 7 — Зачистка и документация
- [ ] 7.1 Удалить VS2017-скрипты/реестровые хаки/мёртвые Framework-таргеты
- [ ] 7.2 Коммит boot-net10 как seed (после зелёного двухстадийного цикла с тестами)
- [ ] 7.3 Обновить AGENTS.md/README под dotnet+net10
- [ ] 7.4 Полный цикл с чистого клона

## Deviations
- 3.1a (TFM net10) блокирован bootstrap chicken-and-egg: boot seed ICE'ит на.ref-сетах net10 (у IntPtr/Void появились generic-интерфейсы; InitSystemTypes резолвит IntPtr до Void — assert TypeInfo.n:551). Порядок: фикс InitSystemTypes в источнике → двухстадийная сборка+тесты → перешивка boot-dnlib с Stage2 (коммит сида — только после зелёного цикла, правило пользователя) → повторный флип TFM. См. progress3.5a-3.1a.md.
- 2.4 разбит: 2.4a (мёртвый UsingTask в Nemerle.Sdk.targets) сделан в Фазе 2; FileUpdate-машинерия Validate и al.exe-разведка перенесены в Фазу 5.1 (они только на классическом хосте, dotnet build не блокируют — см. report2.3-2.4 Q4).
- Пункты плана 2.5.2/2.5.3/2.5.4 частично опровергнуты report2.5: API dnlib 3.3.0→4.5.0 для использованных членов идентичен (0 сломано из 127), `EnableTypeDefFinder`/`FileExists` не существует; приёмка 2.5 = компиляция + Stage2 IL-diff + тесты, не правки резолвера.
- 1.2: `global.json` (SDK 10.0.402) требует MSBuild >= 18: легаси-команда VS2022 msbuild 17.14 из Приложения А больше не работает (MSB4236). Целевое состояние и без того «современный тулчейн»; baseline-док обновлён на vswhere-вход. master не затрагивается (останется на VS2017/4.x).
- 1.3 (проверка ToolsVersion) выполнен досрочно на Фазе 0: git grep показал отсутствие пинов 15; msbuild 17.14.60 собрал Stage1–3 (бейзлайн). Пункт помечен в секции Фазы 0.
- 0.4: Validate на бейзлайне не полностью зелёный — IL-диф 5/5 green, но pefverify падает (старый .NET FX 4.6.1 Tools против netstandard2.0). Зафиксировано как состояние «до»; паритет later фаз = IL-diff + tests, pefverify исключён до Фазы 5.1.

## Journal
- 2026-10-31: ИНЦИДЕНТ: суб-агент 3.1d обрезал NemerleAll.nproj до 0 байт (whole-file rewrite вне правки; сборка к тому моменту уже прошла). Восстановлено из HEAD, правка перенесена точечными заменами, Validate перепроверен (FC ×6). ПРАВИЛО ДЛЯ СУБ-АГЕНТОВ: точечные replace-редакты, после каждого — проверка размера файла и git diff; ReadAllText/WriteAllText/Set-Content по всему файлу запрещены.
- 2026-10-31: 3.1a+3.1c-flatdir закоммичены (2d6fe489f): ncc-core на net10.0, apphost работает без roll-forward (3.6 ✅). Подавление ассерт-диалогов (3.1.1) — c3af18bad, переключатель assertuienabled в .config рядом с exe.
- 2026-10-30: Фаза 2 + 2.5 закрыты (коммиты 952a0a84d, 42d1a188a). Найдена и отложена регрессия System.CodeDom 10.0.14 (codedom.n ArgumentNullException) — бамп отменён, перенесён в 3.3. Отчёт по фазе 3: report3.1-3.5.md (атомы 3.5a→3.1a→3.1b→3.1c→3.4a→3.4b).
- 2026-10-30: Фаза 1 (под-агент): Build-core-base.cmd переведён на vswhere (работает: VS18/MSBuild 18.11), global.json=10.0.402+latestFeature, CheckEolTargetFramework=false (Nemerle.Sdk.props + NemerleAll.nproj, NETSDK1138=0). ВАЖНО: global.json требует MSBuild>=18 — легаси-команда VS2022 msbuild 17.14 изbaseline-дока устарела, запись в baseline-док будет обновлена. Ловушка: cmd.exe съедает запятую в целевой строке без кавычек (`"Stage3,Validate"`) — проверять через полную кавыцированную командную строку.
- 2026-10-30: Ограничение от пользователя: .NET 10 — только ветка `retarget-compiler`; `master` обязана оставаться на **.NET Framework** (не обязательно ровно 4.6.1 — допустима любая версия, совместимая с VS 2017, где работает IDE-плагин). Правки тулчейна — только на retarget, в master не синхронизировать. Библиотеки — netstandard2.0 (D2/D6) в обеих ветках.
- 2026-10-30: Фаза 0 готова: тег `pre-net10-baseline` (d9b50f721); архив seed `C:\RSDN\backup\boot-dnlib-backup-20261030.zip`; бейзлайн Stage1-3 green, Validate IL 5/5 identical, CompilerTests 468+167 passed / 0 failed; рецепт в `doc/NET10-BUILD-BASELINE.md`; логи `C:\RSDN\baseline-logs\`.
