# План: перевод ветки `retarget-compiler` на .NET 10

- **Ветка:** `retarget-compiler`
- **Дата исследования:** 2026-10-30
- **Среда исследования:** Windows, установлены .NET SDK 8/9/10 (`10.0.402`), VS 2017 Community, VS 2022 Professional, VS 2026. Рантаймы .NET: 3.1, 5, 6, 7, 8, 9, 10 (2.1 **нет**).
- **Статус:** черновик v1. Фазы 0–1 не меняют семантику и дают зелёную сборку почти сразу; фазы 2–4 — основной перенос; фазы 5–7 — инструмент/тесты/зачистка.

> Этот план опирается на фактические прогоны сборки ниже, а не на догадки. Каждое утверждение «сломано/работает» проверено командой.

---

## 1. Цель

Перестать собирать и запускать компилятор Nemerle на «древнем дотнете» (.NET Framework 4.6.1 + `msbuild` VS2017 + EOL `netcoreapp2.1`) и перевести компилятор (`ncc`), стандартную библиотеку и инфраструктуру сборки на **.NET 10 (LTS)** с современным тулчейном (`dotnet`/`msbuild` 17+), сохранив **двухстадийный бутстрап** и симметрию семантики с веткой `master` (см. `AGENTS.md`).

Что даём пользователю: рабочий `ncc.exe` на .NET 10, который запускается **без** хаков вида `--roll-forward LatestMajor`; сборку одной командой на современном тулчейне; убирание EOL-предупреждений.

**Жёсткое ограничение обратной совместимости (не нарушать).** Компилятор `ncc` переводим на .NET 10, но **библиотеки, потребляемые извне, обязаны оставаться собранными в `netstandard2.0`** — их грузят хосты на .NET Framework: интеграция с **VS 20xx** (IDE-код живёт на `master` и в `snippets/VS2010/*`), NuGet-пакеты и прочие среды на .NET Framework. Минимальный обязательный набор:
- `Nemerle.dll`
- `Nemerle.Macros.dll`
- `Nemerle.Compiler.dll`
- и любые иные сборки, на которые ссылается IDE / которые выдаются в NuGet (например `Nemerle.Test.Framework`, используемые IDE библиотечные части из `snippets/*`) — **проверить по факту ссылок из VS-интеграции**.

`netstandard2.0` — намеренный знаменатель: он грузится и из .NET Framework 4.6.1+ (в т.ч. VS 20xx, работающем на Framework), и из .NET Core/10 (его грузит net10-хост). Поэтому библиотеки держим на `netstandard2.0` (см. D2/D6); перевод хоста в `net10.0` этого не отменяет. **Запрещено** делать эти библиотеки `net10.0`-only.

**Приоритет.** Первоочерёдно — перевести Nemerle на современный стек сборки (`dotnet`/`msbuild` 17+, net10-хост, `netstandard2.0`-библиотеки) и **забутстрапить** (Stage1→Stage2→fixed point на net10). **Native AOT (`D7`, Фаза 3.7) — факультативно**, только после зелёного бутстрапа, и НЕ является критерием готовности.


## 2. Что обнаружено (эмпирика)

| # | Факт | Как проверено |
|---|---|---|
| E1 | `Build-core*.cmd` падает на этой машине | `Build-core-base.cmd` жёстко тянет VS2017 `msbuild 15.0`; Stage1 падает: `The current .NET SDK does not support targeting .NET Standard 2.0` (Nemerle/Compiler/Macros) и `... .NET Core 2.1` (ncc-core). `EXITCODE=1`. |
| E2 | Тот же Stage1 **успешно** собирается на `msbuild` VS2022 (SDK 10.0.113) | `MSBuild.exe` VS2022 `/t:Stage1` → `EXITCODE=0`; собраны Nemerle.dll, Nemerle.Compiler.dll, Nemerle.Macros.dll, ncc.exe, ncc-core.dll в `bin\Debug\net-4.0\Stage1\`. |
| E3 | **Двухстадийный бутстрап работает уже сейчас** | `msbuild /t:Stage2` → `EXITCODE=0`, выход Stage1 пересобран Stage1-компилятором в `...\Stage2\`. |
| E4 | TFM сейчас: библиотеки `netstandard2.0`; `ncc.nproj` = `net461`; `ncc-core.nproj` = `netcoreapp2.1` | `Nemerle.nproj:18`, `Nemerle.Compiler.nproj:17`, `Nemerle.Macros.nproj:16` = netstandard2.0; `ncc.nproj:16` = net461; `ncc-core.nproj:16` = netcoreapp2.1. |
| E5 | `ncc-core.dll` **не запускается** из коробки | `ncc-core.runtimeconfig.json` зажёсткован на `Microsoft.NETCore.App 2.1.0`; `dotnet ncc-core.dll -help` → «You must install or update .NET… 2.1.0». Работает только `dotnet --roll-forward LatestMajor ncc-core.dll`. Apphost `ncc-core.exe` **не создаётся** (`UseAppHost` выключен по умолчанию для netcoreapp2.1). |
| E6 | Компилятор во время сборки — это **Framework** `ncc.exe` | `Nemerle.MSBuild.targets` использует in-proc MSBuild-таск `Ncc` (`Nemerle.MSBuild.Tasks.dll`), а тот — `ToolTask`, спавнящий `ncc.exe` (`MSBuildTask.cs:12,96-106`). Хост-таск `Nemerle.MSBuild.Tasks.csproj` = `net45`. |
| E7 | Boot-компиллятор (seed) — **Framework** exe | `boot-dnlib\ncc.exe` — управляемый Framework exe (нет `runtimeconfig.json`), набор фасадных `System.*.dll`+`netstandard.dll`+`dnlib.dll`. `NRootBoot=$(NRoot)\boot-dnlib` (`NemerleAll.nproj:24`). |
| E8 | `Nemerle.MSBuild.targets` — «франкенштейн» | `Nemerle.Sdk.props` подключает `Microsoft.NET.Sdk`, но `LanguageTargets` подменён на `Nemerle.MSBuild.targets`, который импортирует старый `Microsoft.Common.targets:38` и использует Framework-резолв ссылок (`GetReferenceAssemblyPaths`/`TargetFrameworkDirectory`/`TargetFrameworkVersion`, строки 218–253). |
| E9 | Пакеты старые; актуальная dnlib сильно новее | `Nemerle.Compiler.nproj`: `dnlib 3.3.0`, `System.CodeDom 4.7.0`; `Nemerle.Compiler.deps.json`: dnlib тянет `System.Reflection.Emit* 4.6.0`. По NuGet актуально **dnlib 4.5.0** (между ними 3.3.5/3.4/3.5/3.6 → 4.0…4.5; крупный мажор 3→4). Nuspec 4.5.0: добавлен TFM **`net6.0`** (без SRE-фасадов), для `netstandard2.0` тянет `System.Reflection.Emit* 4.7.0`. |
| E10 | Тестовый раннер — Framework | `Nemerle.Compiler.Test.nproj` = `net461`; `Nemerle.Test.Framework.nproj` = netstandard2.0. |
| E11 | На ветке **нет** `global.json`/`Directory.Build.props`/CI; `Build-core*.cmd` — основной вход; VS2017-реестр обязателен | Проверено отсутствием файлов; `Build-core-base.cmd:25-36` читает `HKLM\...\SxS\VS7`=15.0. |

## 3. Текущая архитектура сборки (как устроено)

```
Build-core*.cmd → VS2017 VsDevCmd → msbuild NemerleAll.nproj /t:StageN
  Stage1: NRootBoot(boot-dnlib, Framework ncc.exe)  ── компилирует (через Ncc ToolTask) ──► bin/<Cfg>/net-4.0/Stage1
  Stage2: Stage1\ncc.exe                             ── компилирует тех же исходников ──► ...\Stage2   (fixed point проверка через Validate)
  Project-граф Stage1 (NCompilerProject): Nemerle.nproj → Nemerle.Compiler.nproj → Nemerle.Macros.nproj → ncc.nproj(net461) + ncc-core.nproj(netcoreapp2.1)
  Компиляция каждого .nproj: Microsoft.NET.Sdk (Sdk.props/targets) + LanguageTargets=Nemerle.MSBuild.targets → Ncc ToolTask → ncc.exe
  Инфра: Nemerle.MSBuild.Tasks.csproj(net45, ToolTask-хост), MSBuild.Community.Tasks.dll(Framework), misc/keys/*.snk (заглушки по 1 байту)
```

Ключевое: **сборочный граф уже SDK-style и частично на netstandard2.0**, но (а) оркестратор/скрипты привязаны к Framework-`msbuild` VS2017, (б) хост компилятора — Framework `ncc.exe` + net45-таск, (в) Core-артефакт `ncc-core` на EOL-рантайме и без apphost.

## 4. Ключевые проблемы (сводно)

1. **Пин на VS2017** (`Build-core-base.cmd`) — сборка вообще не идёт на современном тулчейне из коробки (E1).
2. **`ncc-core` на `netcoreapp2.1`** — EOL (NETSDK1138), не запускается без roll-forward, нет apphost (E5).
3. **Хост компилятора — Framework** (`ncc.nproj` net461 + net45 `Ncc` ToolTask) — тащит Framework-зависимости в процесс сборки (E6).
4. **Интеграционный слой на Framework**: `Nemerle.MSBuild.targets` поверх `Microsoft.Common.targets` + `MSBuild.Community.Tasks.dll` (Framework) — блокирует `dotnet build` (E8).
5. **Boot-seed — Framework exe** — для настоящего «наследия» на net10 нужен пересеив (бутстрап-критично, E7).
6. **Старые пакеты** (dnlib 3.3.0, System.CodeDom 4.7.0, SRE 4.6.0) (E9).
7. **Тестовый раннер net461** — тесты не идут на Core (E10).
8. **Потенциальные API-разрывы** Core 2.1→10 в коде компилятора/stdlib (см. Фазу 3.5; нижняя оценка риска — сабэджент-сабвей).

## 5. Решения / точки выбора (зафиксировать до начала)

- **D1. Хост сборки: `msbuild.exe` (VS2022) vs `dotnet build`.** Кратко: `dotnet build` НЕ грузит net45-таск `Ncc` и Framework `MSBuild.Community.Tasks.dll`. → Фазы 0–1 идут на `msbuild.exe` 17 (минимум риска), а Фазы 2+ портят слой так, чтобы заработал `dotnet build`. **Рекомендация:** целевое состояние — `dotnet build`.
- **D2. TFM библиотек (жёстко).** `Nemerle.dll`/`Nemerle.Macros.dll`/`Nemerle.Compiler.dll` (и IDE-потребляемые) **остаются на `netstandard2.0`** и НЕ переходят на `net10.0`-only (см. «Жёсткое ограничение» в §1). Хост `ncc` → `net10.0`. Если коду понадобятся net10-only API — **мульти-таргетить** `netstandard2.0;net10.0`, а не менять TFM.
- **D3. Судьба `ncc.nproj` (net461).** На `retarget-compiler` — убрать из сборки (`NCompilerProject`), оставить только Core-хост. На `master` net461 остаётся (кросс-веточное различие, см. `AGENTS.md`).
- **D4. Boot-seed на net10: framework-dependent vs self-contained.** **Рекомендация:** framework-dependent net10 (LTS обязателен на dev/CI). self-contained — резерв, если нужен seed без установленного рантайма.
- **D5. `boot-dnlib` не удалять** до подтверждения fixed point с чистого клона на net10-сиде (см. раздел «Риски»).
- **D6. Обратная совместимость с .NET Framework / VS 20xx (инвариант).** Перечисленные библиотеки обязаны собираться в `netstandard2.0` и грузиться из Framework-хостов (VS 20xx, NuGet). Проверять на приёмке: загрузка из хоста на .NET Framework 4.6.1+ + наличие `netstandard2.0`-выхода в пакетах. См. §1, Фазу 3.2, DoD.
- **D7. Формат exe-хоста (AOT — факультативно, низкий приоритет).** База: `ncc`/`ncc-core` — exe на **.NET 10** (`UseAppHost`, framework-dependent). **Native AOT — вне основного плана**, исследуется только после зелёного бутстрапа (Фаза 3.7); блокираторы — рантайм-выполнение макросов и `System.CodeDom`. Сам по себе «нет установленного рантайма» решает и self-contained apphost без AOT.

## 6. План по фазам

### Фаза 0 — Фиксация и точка опоры (без изменения кода)
- [ ] 0.1 Смешать `git tag` текущего коммита как `pre-net10-baseline`.
- [ ] 0.2 Зафиксировать, что `boot-dnlib/` — рабочий Framework-seed; резервная копия/архив вне дерева сборки.
- [ ] 0.3 Добавить рецепт заведомо рабочей сборки на этой машине (см. Приложение А) и прогнать Stage1+Stage2 как **бейзлайн** (уже зелёные — E2/E3).
- [ ] 0.4 Снять IL-бейзлайн fixed point: прогнать `Stage2; Stage3; Validate` (Validate сравнивает IL двух последних стадий) — зафиксировать «до».
- [ ] 0.5 Прогнать `CompilerTests` (positive+negative) на Framework-раннере — зафиксировать число прохождений как функциональный бейзлайн.
- **Выход:** воспроизводимый зелёный Stage1/Stage2 + бейзлайны Validate и тестов.

### Фаза 1 — Отвязать сборку от VS2017 (без изменения семантики)
- [ ] 1.1 В `Build-core-base.cmd` убрать чтение реестра `SxS\VS7`=15.0; находить `msbuild.exe` через `vswhere -latest -products * -requires Microsoft.Component.MSBuild -find MSBuild\**\Bin\MSBuild.exe` (или `dotnet msbuild` в рамках D1).
- [ ] 1.2 Добавить `global.json` с `sdk.version`=10.0.x (`rollForward: latestFeature`) для детерминизма резолва `Microsoft.NET.Sdk`.
- [ ] 1.3 Убедиться, что `NemerleAll.nproj`/подпроектные вызовы `msbuild` не пинят ToolsVersion=15 и проходят на 17 (E2 уже подтверждает Stage1/Stage2).
- [ ] 1.4 Подавить EOL-шум временно (`<CheckEolTargetFramework>false</CheckEolTargetFramework>`) — НЕ менять TFM на этом шаге, только прогон.
- [ ] 1.5 Прогнать Stage1/Stage2/Validate/CompilerTests — сравнить с бейзлайном Фазы 0 (должно быть идентично).
- [ ] 1.6 (CI) Вернуть workflow (был удалён с ветки): сборка `Stage2` + `Validate` + тесты на `windows-latest` (SDK 10).
- **Выход:** зелёная сборка без VS2017; семантика/артефакты не изменились.

### Фаза 2 — Перенос интеграционного слоя на .NET (открывает `dotnet build`)
Самая кропотливая часть; цель — убрать Framework из самого процесса сборки.
- [ ] 2.1 `tools/msbuild-task/Nemerle.MSBuild.Tasks.csproj`: `net45` → `netstandard2.0` (или `net10.0`); вместо `using Microsoft.Build.Utilities/Framework` — `PackageReference Microsoft.Build.Utilities.Core` (+`Microsoft.Build.Framework`) с `PrivateAssets=all`, `ExcludeAssets=runtime`.
- [ ] 2.2 `MSBuildTask.cs`: `ToolTask` остаётся (есть в `Microsoft.Build.Utilities.Core`), но убрать `Microsoft.Win32.Registry` (`MSBuildTask.cs:8,130-138`) — заменить на передачу пути к `ncc` через MSBuild-свойство (`CompilerPath` уже есть) либо `OperatingSystem`-гейт. Проверить `typeof(Ncc).Assembly.CodeBase` (`MSBuildTask.cs:118`) — на Core может быть null; использовать `Assembly.Location`/`AppContext.BaseDirectory`.
- [ ] 2.3 `tools/msbuild-task/Nemerle.MSBuild.targets`: заменить старый резолв ссылок (`GetReferenceAssemblyPaths`/`TargetFrameworkDirectory`, строки 218–253) на то, что даёт `Microsoft.NET.Sdk` (`@(ReferencePath)` из `PackageReference`/`ProjectReference`/таргетинг-паков). Оставить из файла только «языковой» слой: `CoreCompile` → `Ncc`-таск + разбор вывода (`MSBuildTask.cs` `LogEventsFromTextOutput`). Рассмотреть отказ от `Import Microsoft.Common.targets` (строка 38) в пользу чистой привязки к SDK-цели `CoreCompile` (аналог F#/сторонних языковых SDK).
- [ ] 2.4 Избавиться от `MSBuild.Community.Tasks.dll` (Framework): `UsingTask` в `NemerleAll.nproj:2` и `Nemerle.Sdk.targets:3` (`TemplateFile`, `AL`). Атрибуты сборки уже генерирует SDK (`GenerateAssemblyInfo`, см. `_GenerateAssemblyInfo` в `Nemerle.MSBuild.targets:49`); генерацию policy/config-assembly через `AL` — удалить или переписать на `dotnet`/SDK-задачи.
- [ ] 2.5 Acceptance: `dotnet build Nemerle.nproj`, `Nemerle.Compiler.nproj`, `Nemerle.Macros.nproj` проходят на `dotnet` 10; артефакты ≡ бейзлайну.
- **Выход:** компилятор и библиотеки собираются через `dotnet build`.

### Фаза 2.5 — Обновление dnlib: 3.3.0 → 4.5.0 (независимо от TFM, отдельным коммитом)
Делаем **до** переключения TFM и отдельно от него, чтобы регресс эмитируемого IL был атрибутируем dnlib, а не сменой рантайма (бисектируемость). Актуально: **dnlib 4.5.0** (NuGet); между текущей 3.3.0 и ней — 3.3.5/3.4/3.5/3.6 и крупный мажор **4.0–4.5**.
- [ ] 2.5.1 `Nemerle.Compiler.nproj:37`: `dnlib` 3.3.0 → 4.5.0 (заодно `System.CodeDom` 4.7.0 → актуальный).
- [ ] 2.5.2 Ссылки на SRE: на `netstandard2.0` dnlib 4.5.0 тянет `System.Reflection.Emit* 4.7.0` (было 4.6.0); на net10-хосте (после 3.1) выбирается ассет **`net6.0`** — SRE-фасады уходят. **Явных** `System.Reflection.Emit*` в `.nproj` не добавлять.
- [ ] 2.5.3 **API-поверхность для регресс-проверки** (её использует `ncc/backend/**`; сверить сигнатуры по CHANGELOG/release notes 3.3→4.5 — GitHub API вернул 403, взять вручную): `ModuleDef`/`ModuleDefMD.Load`, `ModuleDef.CreateModuleContext`/`ModuleContext`, `Importer`, `module.CorLibTypes`, `dnlib.DotNet.Emit` (`OpCode`, `CilBody`, `Instruction`), `ModuleWriterOptions`/`NativeModuleWriterOptions` (`WritePdb`, `PdbFileName`, `PEHeadersOptions`, `Cor20HeaderOptions`), `StrongNameKey`+`InitializeStrongNameSigning`, `module.Write()`.
- [ ] 2.5.4 **Резолвер ссылок под .NET Core**: по README dnlib для Core-сборок нужно отключить GAC-загрузку и добавить пути поиска reference-сборок Core через `ModuleContext`/`AssemblyResolver`. Проверить связку с `ncc/backend/hierarchy/LibraryReferenceManager.n` (`Assembly.Load/LoadFrom`, строки 145/146/150/282).
- [ ] 2.5.5 **Strong-name**: путь `StrongNameKey`+`InitializeStrongNameSigning` не сломать с учётом того, что `misc/keys/*.snk` — 1-байтные заглушки (реальная подпись, вероятно, не применяется).
- [ ] 2.5.6 **PDB**: portable PDB — из коробки; Windows PDB требует доп. пакет `Microsoft.DiaSymReader.Native` (только Windows, опционально). Зафиксировать, какой формат эмиттит компилятор, чтобы не потерять отладку.
- [ ] 2.5.7 Acceptance: Stage1 собирается с новой dnlib; `CompilerTests` не хуже бейзлайна Фазы 0.5; прогнать `Validate`-подобный IL-диф `Stage1(dnlib 4.5.0)` vs `Stage1(dnlib 3.3.0)` и подтвердить, что отличия эмиттера ожидаемы.
- **Откат/компромисс:** если мажор 4.x ломает emit дороже, чем оправдано, — зафиксироваться на последней 3.x (3.6.0) и отложить 4.x на отдельную ветку.
- **Выход:** компилятор на актуальной dnlib; регресс эмиттера отсутствует или объяснён.

### Фаза 3 — Перенос хоста компилятора и библиотек на .NET 10
- [ ] 3.1 `ncc-core.nproj`: `netcoreapp2.1` → `net10.0`; включить `UseAppHost=true`; при необходимости `RuntimeIdentifier`. Цель — получить запускаемый `ncc-core.exe` (apphost) + `runtimeconfig.json` на 10.0.
- [ ] 3.2 По D2/D6 библиотеки держим на `netstandard2.0` (НЕ `net10.0`-only): `Nemerle.nproj`, `Nemerle.Macros.nproj`, `Nemerle.Compiler.nproj` (+ IDE-потребляемые). Проверить, что (а) net10-хост грузит эти netstandard2.0-сборки, и (б) те же dll грузятся из .NET Framework 4.6.1+/VS 20xx. При нужде в net10-only API — мульти-таргет `netstandard2.0;net10.0` и учесть, что `Nemerle.Sdk.props:5` ставит `AppendTargetFrameworkToOutputPath=false` (для двух TFM нужны отдельные подпапки вывода).
- [ ] 3.3 Пакеты: `System.CodeDom` → актуальный; **не** добавлять явных `System.Reflection.Emit*` (inbox на net10). Обновление `dnlib` вынесено в Фазу 2.5.
- [ ] 3.4 Убрать `ncc.nproj` (net461) из `NCompilerProject` (`NemerleAll.nproj:56-61`) по D3.
- [ ] 3.5 **API-разрывы Core 2.1→10 в коде компилятора/stdlib** — по сабвею (реальных точек ~15; живого `System.Reflection.Emit`/`BinaryFormatter`/`Registry` в `.n`-коде нет; эмит — через dnlib). По критичности:

| Файл:строка | Проблема | Sev | Правка |
|---|---|---|---|
| `ncc/main.n:52` | `Assembly.CodeBase` → `PlatformNotSupportedException` с Core 3.0 → **крах ncc на старте** | H | `AppContext.BaseDirectory`/`Assembly.Location` |
| `ncc/backend/hierarchy/LibraryReferenceManager.n:123-143,159,164,199` | `getAssemblyLocation`/`CodeBase` + gac/mono-хак пути | H | `Assembly.Location`; убрать хак |
| `ncc/backend/hierarchy/LibraryReferenceManager.n:145-155,282` | `Assembly.Load(name[,Version])/LoadFrom` — на Core probing по deps.json; pin `Version=` падает | H | `LoadFrom`/`AssemblyLoadContext.LoadFromAssemblyPath`; снять pin |
| `ncc/frontend/ManagerClass.n:565-569` | дефолтный набор ссылок из `typeof(object/Uri/XmlNode).Assembly.Location` → на Core это impl-ассембли `System.Private.*`, не ref-set | H | резолвить ref-пак `Microsoft.NETCore.App` (как Roslyn) |
| `tools/msbuild-task/MSBuildTask.cs:103,118,131` | таск: `Assembly.CodeBase`, `Registry` «App Paths», `ToolName="ncc.exe"` (Core=`ncc.dll`+apphost) | H (Фаза 2) | `Assembly.Location`; гейт `OperatingSystem.IsWindows()`; имя тула по ОС |
| `ncc/frontend/codedom/*` + `Nemerle.Compiler.nproj:38` | `CodeDomProvider`/`ICodeCompiler` из `System.CodeDom 4.7.0` (legacy) | M | bump `System.CodeDom` на 9/10 или удалить поверхность (см. и AOT-блокер 3.7) |
| `macros/Resource.n:68` | рефлекс-загрузка pinned `System.Windows.Forms 4.0.0.0` для resx → тихо ломает resx-макрос | M | `System.Resources.Extensions`/свой парсер |
| `lib/hashtable.n:103` (+ 8 `[Serializable]`) | ctor `(SerializationInfo, StreamingContext)` → `SYSLIB0050/0051` как ошибки на .NET 9/10 | M | удалить/гейтить ctor, атрибуты оставить |
| `lib/AssemblyInfo.n:47`, `AttributeCompilerClass.n:121-148`, `TypeBuilder.n:2048` | CAS/APTCA/permission-set — на Core нет семантики (эмит безвреден) | L | эмит оставить, по семантике не полагаться |
| `ncc/frontend/codedom/NemerleCodeCompiler.n:43`, `LibraryReference.n:84` | неиспользуемый `using System.Configuration`; `SHA1Managed()` | L | убрать using / `SHA1.Create()` |
| `ncc/frontend/Makefile:293,320`, `macros/macros.build:50` | `-r System.Data -r System.Windows.Forms` (FX-ассембли нет на Core) | M | убрать/ref-пак (не основной Makefile-путь) |

  - **0 опасностей:** глобализация/энкодинги (парсер на `CultureInfo.InvariantCulture`), трэд-культура/`Thread.Abort`, прямой P/Invoke/`Marshal` в `.n`-коде — тихие runtime-поведения для самого компилятора маловероятны.
  - **Символы компиляции:** в `.n`-коде нет веток по рантайму (`#if NETSTANDARD/NETCOREAPP/MONO/RUNTIME_MS`); `NET_4_0` определён глобально (`NemerleAll.nproj:34,38`) и нужен **только testsuite** — продолжать определять. Новый символ хоста (`NET`/`NET10`) при надобности — **ввести**.
  - Синергия с D2/D6: сохранение `netstandard2.0` снимает риск смены ref-set `lib/`/`Nemerle.Compiler`.
  - **Оценка: Medium-High** — поверхность мала, но ядро (резолвинг ссылок/локация ассембли + net10-seed) нагрузочное; обязательна ревалидация fixed point Stage1/Stage2.
- [ ] 3.6 Acceptance: `bin/.../Stage1/ncc-core.exe -version` запускается **без** `--roll-forward` на этой машине.
- [ ] 3.7 **AOT (стретч, по D7).** Базово — framework-dependent apphost (3.1). Отдельным экспериментом пробовать **Native AOT** (`PublishAot=true` + `RuntimeIdentifier`) для `ncc`, чтобы получить самодостаточный нативный `ncc.exe` без установки рантайма. **Блокираторы AOT проверить заранее:** (а) `dnlib` — вероятно совместима (пишет метаданные потоками, не эмитит рантайм-IL; использовать `net6.0`/`net8.0`-ассет, не SRE-путь); (б) **выполнение макросов** — ncc грузит и вызывает произвольные macro-ассембли в рантайме, что противоречит AOT/trimming (главный стоп-фактор); (в) `System.CodeDom`/`CodeDomProvider` (рантайм-кодген) — не-AOT; (г) `Assembly.Load`/ALC-резолвинг ссылок. Если AOT не проходит — остаться на framework-dependent/self-contained apphost (это уже решает «нет установленного рантайма» через self-contained).
- **Выход:** есть компилятор на net10, который реально запускается; AOT либо включён, либо явно отложен с указанием блокиратора.

### Фаза 4 — Пересев бутстрапа на .NET 10 (бутстрап-критично)
Строго по модели двухстадийного бутстрапа из `AGENTS.md`.
- [ ] 4.1 Собрать Stage1 **старым Framework-seed** (`boot-dnlib`) как раньше → на выходе `ncc` на net10 (из Фазы 3).
- [ ] 4.2 Сформировать новый seed-каталог `boot-net10/`: net10 `ncc.exe`(apphost)+`Nemerle.Compiler.dll`+`Nemerle.dll`+`Nemerle.Macros.dll`+`dnlib.dll`+`runtimeconfig.json`(10.0) (+зависимости). Формат по D4 (framework-dependent).
- [ ] 4.3 Проверка бутстрапа: перевести `NRootBoot` (`NemerleAll.nproj:24`) на `boot-net10` (через свойство/ветку-флаг, не ломая master-путь) и прогнать `Stage1→Stage2→Stage3→Validate`; **fixed point = IL Stage2 ≡ Stage3**. Это доказывает, что net10-компилятор воспроизводимо собирает себя.
- [ ] 4.4 Убедиться, что `Ncc` ToolTask спавнит net10 `ncc.exe` (имя/поиск `ToolName`/`CompilerPath`); спавн процесса не зависит от рантайма хоста.
- [ ] 4.5 `boot-dnlib` держать как аварийный seed, пока fixed point не воспроизведён с **чистого клона** на `boot-net10` (см. «Риски»).
- **Выход:** seed на net10, self-host подтверждён fixed point.

### Фаза 5 — Оркестрация и скрипты
- [ ] 5.1 `NemerleAll.nproj`: убрать Framework-SDK-разведку (`InitTools`: `GetFrameworkSdkPath`/`SDKBin`/`gacutil`/`ildasm`/`al.exe`/`ngen`/`peverify`, строки 151–197) — не нужна на Core; `al.exe`/policy-файлы удалить.
- [ ] 5.2 Переписать `Build-core*.cmd` (и добавить `build.ps1`/`build.sh`): `dotnet restore/build`, без `VsDevCmd`/реестра VS; кросс-платформенно (исторически был Linux/`xbuild`).
- [ ] 5.3 Ввести `Directory.Build.props` (общий `TargetFramework`/`LangVersion`/`Nullable`/`TreatWarningsAsErrors`) и `nuget.config`. Рассмотреть настоящий `.sln`/`.slnx` вместо `NemerleAll.nproj` как оркестратора.
- [ ] 5.4 `ExternalDependences/`: осовременить/убрать `NuGet.exe`, `MSBuild.Community.Tasks.dll`, NUnit-консоль (Framework). WiX-инсталлятор оставить изолированно (Windows/Framework, вне ядра компилятора).
- [ ] 5.5 `_Wpf`/WPF и `snippets/VS2010/*` (`_Integration`) — гейтить по Windows/TFM или исключить из Core-сборки компилятора.
- **Выход:** сборка/тест одной командой `dotnet`, без VS-специфики.

### Фаза 6 — Тесты и валидация
- [ ] 6.1 Перевести тестовый раннер `Nemerle.Compiler.Test.nproj` `net461` → `net10.0` (это Nemerle-код, собирается самим `ncc`); `Nemerle.Test.Framework` — по D2.
- [ ] 6.2 `CompilerTests` (`NemerleAll.nproj:349`): запуск `Nemerle.Compiler.Test.exe` через apphost/`dotnet`; сравнить число прохождений с бейзлайном Фазы 0.5 ( parity).
- [ ] 6.3 CI: Stage1→Stage2→Stage3 + Validate (fixed point) на net10 + smoke `ncc-core.exe -version` без roll-forward.
- [ ] 6.4 Runtime-behavior проверки: dnlib-emit, `System.CodeDom`, инвариантная глобализация (`InvariantGlobalization=false`), регистрация `CodePagesEncodingProvider` (в netstandard-фасадах seed'а много энкодингов — компилятор/stdlib могут полагаться на Legacy CodePages).
- **Выход:** тесты идут на Core, паритет с бейзлайном.

### Фаза 7 — Зачистка и документация
- [ ] 7.1 Удалить VS2017-скрипты/реестровые хаки и мёртвые Framework-таргеты.
- [ ] 7.2 Закоммитить свежий `boot-net10` как seed ветки; описать двухseed-переходный период.
- [ ] 7.3 Обновить `AGENTS.md` (разделы Build/Bootstrapping) и `README` под `dotnet`-тулчейн и net10.
- [ ] 7.4 Прогнать полный цикл с чистого клона (clean checkout → seed net10 → Stage1→Stage2→Validate→tests).

## 7. Риски и безопасность бутстрапа

- **Не потерять seed.** `boot-dnlib` — единственный способ собрать ветку, пока net10-seed не подтверждён. Удаление/перезапись seed'а до воспроизведения fixed point с чистого клона = нерабочая ветка (см. `AGENTS.md`, «Bootstrapping»). Два seed'а сосуществуют на переходе — это штатно (в репо уже два: `boot-4.0`, `boot-dnlib`).
- **Fixed point обязателен.** «Компилируется бустом (Stage1)» ≠ «корректен». Только Stage2/Stage3 + `Validate` (IL-диф) доказывают корректность и воспроизводимость.
- **Порядок Фаз 2↔3.** Пока `Ncc`-таск/Framework-интеграция не перенесены (Фаза 2), `dotnet build` не заработает; не пытаться «просто сменить TFM» в обход — иначе Эрик-зависимый `Microsoft.Common.targets`-резолв ссылок не даст корректных ref-паков net10.
- **Симметрия с `master`.** Правки языка/семантики — в обе ветки; TFM/тулчейн — только `retarget-compiler` (см. `AGENTS.md`).
- **Сильные имена.** `misc/keys/*.snk` — заглушки по 1 байту; не вводить требование реального ключа по ошибке при переносе проектов.
- **Глобализация/энкодинги по умолчанию** отличаются на net10 (инвариантная культура, отсутствие CodePages по умолчанию) — высокий риск тихих поведений, покрыть Фазой 6.4.
- **Мажор dnlib 3→4** меняет поверхность эмиттера — это риск тихой порчи IL (корректные, но иные сборки). Поэтому обновление вынесено в Фазу 2.5 отдельным коммитом с IL-диффом и прогоном тестов до смены TFM.

## 8. Критерии готовности (Definition of Done)

1. `dotnet build` (или `msbuild` 17 без VS2017) собирает Stage1 и Stage2 на net10.
2. `ncc` на .NET 10 запускается без `--roll-forward`; `ncc-core.exe` (apphost) присутствует.
3. `Validate` даёт fixed point на net10 (Stage2 IL ≡ Stage3 IL).
4. `CompilerTests` (positive+negative) не хуже бейзлайна Фазы 0.
5. `NETSDK1138` (EOL) отсутствует; `dnlib` актуальна (4.5.0, см. Фазу 2.5), `System.CodeDom` актуален.
6. `Nemerle.dll`/`Nemerle.Macros.dll`/`Nemerle.Compiler.dll` собираются в `netstandard2.0` и грузятся из хоста на .NET Framework 4.6.1+ (VS 20xx-совместимость, D6).
7. Хост `ncc` — exe на .NET 10 (запускается без `--roll-forward`). *(Native AOT — вне DoD, факультативно: см. D7 / Фазу 3.7.)*
8. Чистый клон + `boot-net10` → полный цикл зелёный.

## Приложение А. Команды, работающие прямо сейчас (проверено)

```powershell
# Stage1 (одна стадия) — успех на VS2022 msbuild / SDK 10 (VS2017-скрипт НЕ работает):
& 'C:\Program Files\Microsoft Visual Studio\2022\Professional\MSBuild\Current\Bin\MSBuild.exe' `
  'F:\RSDN\nemerle\NemerleAll.nproj' /t:Stage1 /p:NTargetName=Build /p:Configuration=Debug `
  /p:NInstall='F:\RSDN\nemerle\bin\Debug\net-4.0' /v:m
# Stage2 (две стадии, self-host) — тот же вызов с /t:Stage2. Оба = EXITCODE 0.

# Проверка, что Core-хост ЕДВА запускается (это и есть «проблема использования»):
dotnet --roll-forward LatestMajor F:\RSDN\nemerle\bin\Debug\net-4.0\Stage1\ncc-core.dll -help
# Без --roll-forward: «You must install or update .NET ... Microsoft.NETCore.App 2.1.0».
```

## Приложение Б. Карта файлов правок

- Тулчейн/оркестр: `Build-core-base.cmd`, `Build-core*.cmd`, `NemerleAll.nproj` (`InitTools`/`NTasks`/`Stage*`/`CompilerTests`/`Install`), +новые `global.json`/`Directory.Build.props`/`nuget.config`/CI.
- Интеграция сборки: `tools/msbuild-task/Nemerle.MSBuild.tasks.csproj`, `Nemerle.MSBuild.targets`, `Nemerle.Sdk.props`, `Nemerle.Sdk.targets`, `MSBuildTask.cs`.
- TFM/пакеты: `Nemerle.nproj`, `Nemerle.Compiler.nproj`, `Nemerle.Macros.nproj`, `ncc.nproj`, `ncc-core.nproj`, `snippets/Nemerle.Test/*`.
- Код под починку API: `ncc/frontend/**`, `ncc/backend/generation/**` (+ `lib/`, `macros/` по сабвею).
- Бутстрап: `boot-dnlib/` (seed, не трогать до подтверждения), новый `boot-net10/`, `NemerleAll.nproj:24` (`NRootBoot`).
