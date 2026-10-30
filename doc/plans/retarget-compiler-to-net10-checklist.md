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
- [ ] 2.1 `Nemerle.MSBuild.Tasks.csproj`: net45 → netstandard2.0/net10.0 + PackageReference Microsoft.Build.*
- [ ] 2.2 `MSBuildTask.cs`: убрать Registry, CodeBase→Location/AppContext.BaseDirectory
- [ ] 2.3 `Nemerle.MSBuild.targets`: SDK-резолв ссылок вместо GetReferenceAssemblyPaths; пересмотр Import Common.targets
- [ ] 2.4 Убрать `MSBuild.Community.Tasks.dll` (TemplateFile/AL → SDK-механизмы)
- [ ] 2.5 Acceptance: `dotnet build` Nemerle/Compiler/Macros .nproj, артефакты ≡ бейзлайну

## Фаза 2.5 — dnlib 3.3.0 → 4.5.0 (отдельный коммит)
- [ ] 2.5.1 dnlib 4.5.0 + System.CodeDom актуальный в Nemerle.Compiler.nproj
- [ ] 2.5.2 Не добавлять явных System.Reflection.Emit*
- [ ] 2.5.3 Сверить API-поверхность dnlib 3.3→4.5 (ncc/backend/**)
- [ ] 2.5.4 Резолвер ссылок под Core (ModuleContext/AssemblyResolver, LibraryReferenceManager.n)
- [ ] 2.5.5 StrongNameKey путь не сломать (snk — заглушки)
- [ ] 2.5.6 Зафиксировать формат PDB
- [ ] 2.5.7 Acceptance: Stage1 с новой dnlib; CompilerTests не хуже бейзлайна; IL-диф 4.5.0 vs 3.3.0 объяснён

## Фаза 3 — Хост компилятора и библиотеки на .NET 10
- [ ] 3.1 `ncc-core.nproj`: net10.0 + UseAppHost → запускаемый ncc-core.exe
- [ ] 3.2 Библиотеки остаются netstandard2.0 (D2/D6); проверить загрузку из net10 и Framework 4.6.1+
- [ ] 3.3 Пакеты: System.CodeDom актуальный; явных SRE* не добавлять
- [ ] 3.4 Убрать ncc.nproj (net461) из NCompilerProject (D3)
- [ ] 3.5 Починить API-разрывы Core 2.1→10 по таблице (main.n CodeBase, LibraryReferenceManager, ManagerClass ref-set, codedom, Resource.n, hashtable.n и др.)
- [ ] 3.6 Acceptance: `Stage1/ncc-core.exe -version` без `--roll-forward`
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
- 1.2: `global.json` (SDK 10.0.402) требует MSBuild >= 18: легаси-команда VS2022 msbuild 17.14 из Приложения А больше не работает (MSB4236). Целевое состояние и без того «современный тулчейн»; baseline-док обновлён на vswhere-вход. master не затрагивается (останется на VS2017/4.x).
- 1.3 (проверка ToolsVersion) выполнен досрочно на Фазе 0: git grep показал отсутствие пинов 15; msbuild 17.14.60 собрал Stage1–3 (бейзлайн). Пункт помечен в секции Фазы 0.
- 0.4: Validate на бейзлайне не полностью зелёный — IL-диф 5/5 green, но pefverify падает (старый .NET FX 4.6.1 Tools против netstandard2.0). Зафиксировано как состояние «до»; паритет later фаз = IL-diff + tests, pefverify исключён до Фазы 5.1.

## Journal
- 2026-10-30: Фаза 1 (под-агент): Build-core-base.cmd переведён на vswhere (работает: VS18/MSBuild 18.11), global.json=10.0.402+latestFeature, CheckEolTargetFramework=false (Nemerle.Sdk.props + NemerleAll.nproj, NETSDK1138=0). ВАЖНО: global.json требует MSBuild>=18 — легаси-команда VS2022 msbuild 17.14 изbaseline-дока устарела, запись в baseline-док будет обновлена. Ловушка: cmd.exe съедает запятую в целевой строке без кавычек (`"Stage3,Validate"`) — проверять через полную кавыцированную командную строку.
- 2026-10-30: Ограничение от пользователя: .NET 10 — только ветка `retarget-compiler`; `master` обязана оставаться на **.NET Framework** (не обязательно ровно 4.6.1 — допустима любая версия, совместимая с VS 2017, где работает IDE-плагин). Правки тулчейна — только на retarget, в master не синхронизировать. Библиотеки — netstandard2.0 (D2/D6) в обеих ветках.
- 2026-10-30: Фаза 0 готова: тег `pre-net10-baseline` (d9b50f721); архив seed `C:\RSDN\backup\boot-dnlib-backup-20261030.zip`; бейзлайн Stage1-3 green, Validate IL 5/5 identical, CompilerTests 468+167 passed / 0 failed; рецепт в `doc/NET10-BUILD-BASELINE.md`; логи `C:\RSDN\baseline-logs\`.
