# План: перевод nitra и TDL на компиляцию с топа `retarget-compiler` (nemerle, net10)

Дата: 2026-10-31. Режим: orchestrator + по одному суб-агенту за раз (как в фазах 0-9 nemerle).
Цепочка зависимостей: **TDL → nitra → nemerle**.

## Цель и приёмка (определяет пользователь)

Цепочка собирается сверху вниз со свежих топов:
- nemerle @ `retarget-compiler` (готов: drop `C:\RSDN\nemerle\bin\Debug\net-4.0`, FC ×10 + сьют зелёные);
- nitra @ `kl/retarget-nitra` собирается этим nemerle и собирает свои артефакты (Stage-выходы);
- **Финальная приёмка: TDL @ `kl/retarget-tdl` собирается на .NET 10 и все тесты TDL проходят.**

## Факты разведки (проверено, 2026-10-31)

- Макет путей — штатный: `src/Directory.Build.props` (TDL) и `Nitra.MSBuild.Sdk/Sdk.props` (nitra) резолвят
  `NitraAndNemerleSourcesPath` = `C:\RSDN\` → `nitra\bin\Debug\Stage1`, `nitra\Grammars\bin\Debug`,
  `nemerle\bin\Debug\net-4.0\Stage1`; переопределение — `src\Nitra.user`. Реальный макет
  (`C:\RSDN\nemerle|nitra|TDL`) совпадает. `C:\RSDN` — симлинк на `F:\RSDN`.
- **nitra**: 1184 `.n`-файла; бутстрап-цепочка с проверяемыми бинарными `Boot1/`, `Boot2/`
  (в Boot1 лежит Nemerle-тулчейн прошлого поколения — `Boot1/Net-4.0/Nemerle.MSBuild.targets`);
  `Common/NitraBuild.targets` использует `$(Nemerle)\Nemerle.MSBuild.Tasks.dll`
  (`Nemerle.Tools.MSBuildTask.GetGitTagRevision`) и версию `$(Nemerle)\Nemerle.dll`.
  Целевая ветка `kl/retarget-nitra` содержит dnlib-бэкенд (`DotNet.BackEnd.Dnlib/`) — т.е. это и есть
  core-ветка nitra. Локальная `retarget-nitra` отстаёт от kl на 23 коммита (0 собственных → безопасный ff).
  HEAD сейчас на `test` (не наш рабочий вход); untracked `AI/`, `diff.txt` не трогать.
- **TDL**: `kl/retarget-tdl` уже ретаргечен на Core: `src\TdlCompiler` (`tdlc`, net5.0),
  `Tdl`/`Tdl2JsonLib` — netstandard2.0 на Nemerle-макропроектах, Nitra-бэкенд dnlib; golden-тесты
  (`Tdl.Tests`) гоняют каждый кейс двумя тулчейнами (net461 `tdlc.exe` + `dotnet tdlc.dll`); вход —
  `src\Build.cmd` (vswhere `-version 16.0` — устарел, на машине VS с MSBuild 18). По AGENTS.md TDL
  «сейчас не собирается»: nitra Stage1 — старая net461/CCI-сборка. Ветки-роли: `master` = FX/IDE
  (не трогать), `retarget-tdl` = Core/CI; правило TFS: каждый коммит master мержится в retarget-tdl (не наш
  сценарий — мы коммитим сразу в retarget-ветку).
- nemerle-потребность потребителей: drop с `Nemerle.MSBuild.Tasks.dll`, `Nemerle.dll`, Sdk-props/targets —
  всё есть в текущем net10-drop; НО потребители писались против ПРОШЛОГО поколения retarget-droпов
  (ncc.exe/netcoreapp2.1-era), а текущий топ: `ncc-core.exe`, net10, двусторонний Validate и т.д. — инвентарь
  несовпадений делает N0.

## Фазы и атомы (чек-лист — в `nitra-tdl-retarget-checklist.md`)

### Фаза N0 — Research (без изменений)
- N0.1 **nitra-инвентарь**: состояние `kl/retarget-nitra` от точки ff: точки входа сборки
  (`Build*.cmd`), цепочка Boot1/Boot2 (какое поколение тулчейна в них — netstandard-эра? dnlib?), чем
  Consumer-API nemerle в этой ветке отличается от текущего топа nemerle (пути дропа, ncc.exe vs ncc-core,
  `Nemerle.MSBuild.targets`/Sdk-props по диффу nemerle merge-base→HEAD, TFM-ожидания, `NemerleBinPathRoot`),
  что из boot-дропа nitra придётся перешивать (аналог nemerle 4.6). Проверяемость: команды сборки,
  которые агент может прогнать Read-only probe (dry-run/один прогон — с оглядкой на запрет двойных сборок).
- N0.2 **TDL-инвентарь**: состояние `kl/retarget-tdl`: TFMs (`net5.0` — CEOL; bump до `net10.0` как часть
  приёмки «на .NET 10»), `src\Build.cmd` (vswhere 16.0 → современный вход или `NemerleBinPathRoot`- Override),
  паритет тестового контура (golden `Tdl.Tests` оба тулчейна + MSTest Transformator), что значит «все тесты
  проходят» (числа baseline, если что-то заведомо красное — фиксировать).
- Выход N0: два отчёта + уточнённая атомизация N1/N2 в чек-листе.

### Фаза N1 — nitra на новом nemerle (ветка `retarget-nitra`, ff до kl)
- N1.x (по N0.1): перевар wiring-несовпадений (пути дропа/ncc-core/tasks/Sdk-props) → сборка бутстрап-цепочки
  nitra (Boot→Stage1) свежим nemerle → перешивка boot-дропа nitra, если он несовместим (правило как в nemerle:
  коммит сида отдельным коммитом после зелёного прогона) → полный билд nitra +Grammar+LanguageCompiler →
  тесты nitra (её suite; baseline-норма фиксируется в N0.1).
- Гейт атомов: сборка+nitra-тесты; коммиты только в `retarget-nitra`; в nemerle — ничего (если вскроется баг
  компилятора — отдельный атом в nemerle со своими гейтами FC ×10 + сьют).

### Фаза N2 — TDL на новой nitra (ветка `retarget-tdl`)
- N2.x (по N0.2 + готовой N1): bump TFMs потребителей на актуальные (net10 для tdlCompiler там, где приёмка
  требует «на dotnet 10»; ns2.0-библиотеки — по D6-аналогу), современный вход сборки, прогон golden-тестов
  обоими живыми тулчейнами.
- **Гейт = приёмка пользователя: `TDL собирается на dotnet 10 и все тесты проходят`** (Tdl.Tests golden +
  Tdl.Transformator.Tests; числа в чек-лист).

### Фаза N3 — Зачистка/доки
- Обновить TDL `AGENTS.md` (раздел «Ветка retarget-tdl»: актуальные тулчейны/входы вместо net5.0/VS2019),
  доки nitra где нужно; отчёт-резюме; Deviations.
- **Пуш везде — только по явному решению пользователя** (kl = TFS, github — наружу).

## Общие правила исполнения

- Один суб-агент за раз; один билд за раз во ВСЕЙ системе (сборки фазы 9 nemerle и N1/N2 не пересекаются по
  времени; оркестратор ждёт завершения MSBuild-процессов перед запуском следующего билда).
- Скретч/логи: `C:\RSDN\nemerle\.tmp\nitra-tdl\phaseN*\`. `git grep`, без Select-String. Правки — точечные.
- В nemerle-репозитории потребители не собирают nemerle (используют готовые дропы); если чей-то вход
  триггерит rebuild nemerle — переопределять на дроп, не пересобирать.
- Ничего не пушим. В nitra не трогать untracked (`AI/`, `diff.txt`) и ветку `test`. В TDL — чистый master как есть.
- Правила веток потребителей: в `retarget-tdl` не носить net461/VSIX-частное (AGENTS.md TDL); FX-часть
  (`master` обеих веток) не трогать вообще.

## Открытые вопросы (у устном решении по ходу)

1. Bump `net5.0` → `net10.0` в `src\TdlCompiler` считаем требуемым приёмкой («на dotnet 10»); если
   suffices roll-forward без смены TFM — зафиксируем в N0.2 и согласуем.
2. Норма «все тесты проходят» для golden-тестов, которые рождались под FX: эталоны JSON не меняем —
   расхождения вывода = дефект тулчейна, чиним тулчейн (не эталон), если не зафиксируем обратное в N0.
