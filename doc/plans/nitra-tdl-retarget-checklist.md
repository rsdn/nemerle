# Чек-лист: перевод nitra и TDL на nemerle@retarget-compiler

План: `nitra-tdl-retarget-plan.md`. Приёмка: **TDL собирается на dotnet 10, все тесты TDL зелёные**.
Порядок: строго один атом, один суб-агент, один билд в системе.

## Фаза N0 — Research
- [✅] N0.1 report-n0-1-nitra.md: цепочка Build.cmd→BootTasks BuildBoot (Boot1 FX-сид 2019-года)→Nitra-Stagt1.sln; consumer-API топа совместен as-is (tasks/namespace/Sdk-props/ncc-auto-remap) кроме: VS2017-пин во входах (обходится прямым msbuild + MSBUILDENABLEALLPROPERTYFUNCTIONS=1), ShiftBoot NccFile-список (ncc.exe/32/64 — блокер только для reseed). ПРОБА: BuildBoot зелёный; Stage1 за 6 мин — 3 ошибки в 2 проектах: Nitra.ClientServer.Server (resx-макро = FX-only, наш 7.5!) + VSIX-легаси (вне скоупа). Все три TDL-входа (Stage1/Grammars/LanguageCompiler) СОБРАНЫ. Атомизация N1.1-N1.9 в репорте
- [✅] N0.2 report-n0-2-tdl.md: in-scope проекты/вход разобраны; vswhere-пин Build.cmd жив (range ≥16.0), реальные дыры: packages.config-фид (src\packages gitignored) и T4 в VS dev env. **Bump net5.0→net10.0 в TdlCompiler доказан как требуемый**: на установленном .NET 5.0.17 Core-нога AV'ит 72/72 (0xC0000005), bump → **144/144** (+ константа пути net5.0\ в тесте); net461-нога собирается и проходит на MSBuild 18.11 — остаётся. Transformator.Tests 20/20. Кросс-тулчейн: 57/72 байт-идентичны, 6 различий = порядок Dictionary/HashSet → per-leg эталоны обязательны. КЛЮЧЕВОЕ: golden-сверка на ветке inert (NetFramework-/NetCore-эталоны никогда не коммитились → всё живёт в message-mode; форс коммиченых эталонов = 130/144 fail, pre-existing, от тулчейна не зависит). Блокеров consumer-API нет (resx-класс не срабатывает — отсутствующих resx нет)
- [✅] N0.3 Уточнение: N2-атомы приняты по репорту (N2.1 bump+константа → 144/144; N2.2 restore/вход; N2.3 DISPOSITION golden = остаёмся в message-mode как есть, перогенерацию per-leg эталонов НЕ делаем — вне рамок приёмки «все тесты проходят», зафиксировано как отклонение; N2.4 финальный цикл через Build.cmd 144/144+20/20 на dotnet 10; N2.5 гигиена опционально). N1-атомы N1.1..N1.9 без изменений, ff nitra-ветки (N1.1) объединяется с N1.2 в один агент

## Фаза N1 — nitra (ветка `retarget-nitra`, ff до `kl/retarget-nitra`) — атомизация по report-n0-1
- [ ] N1.1 ff локальной `retarget-nitra` до kl (23 коммита, ff безопасен)
- [ ] N1.2 Адаптация входа сборки: убрать VS2017-реестровый пин, прямой современный msbuild + drop-root wiring (гейт: задокументированная команда зелёная)
- [ ] N1.3 Nitra.ClientServer.Server resx-макро: порт проекта с resx-макро или исключение из core-набора (гейт: компилируется ncc-core или исключён)
- [ ] N1.4 Core-набор сборки без VSIX/IDE-легаси: BuildBoot+Stage1 zero-error (~7 мин гейт)
- [ ] N1.5 Тесты nitra: baseline `Tests\run-positive.cmd`/`run-negative.cmd` (35+20 корпус, харнесс = Nemerle.Compiler.Test.exe из дропа) — точные числа в чек-лист
- [ ] N1.6 Grammars + LanguageCompiler smoke (Calculator через Stage1-тулчейн)
- [ ] N1.7 ShiftBoot NccFile → ncc-core-сет + флип boot-nproj — валидация во временном worktree
- [ ] N1.8 Reseed-коммит Boot1/Boot2 (+version.xml) — отдельный бинарный коммит, гейт: чистый чекаут BuildBoot+Stage1+тесты зелёные
- [ ] N1.9 Проверка handoff для N2: три TDL-выхода удовлетворяют Nitra.MSBuild.Sdk/Sdk.props (по N0.1 — да; подтвердить фактическими артефактами)

## Фаза N2 — TDL (ветка `retarget-tdl`) — атомизация по report-n0-2
- [ ] N2.1 Bump `TdlCompiler` net5.0→net10.0 + константа пути в тесте (гейт: 144/144 Core-нога)
- [ ] N2.2 Восстановление/вход: packages.config-фид и T4 в прогонном входе (гейт: Build.cmd-путь зелёный)
- [~] N2.3 Golden-сверка: DISPOSITION — остаёмся в message-mode (штатный режим ветки); per-leg перогенерацию эталонов не делаем (зафиксировано в плане)
- [ ] N2.4 ФИНАЛЬНАЯ ПРИЁМКА: чистый прогон через Build.cmd → `Tdl.Tests` 144/144 (обе ноги) + `Tdl.Transformator.Tests` 20/20 на dotnet 10
- [ ] N2.5 (опц.) гигиена

## Фаза N3 — Зачистка/доки
- [ ] N3.1 TDL AGENTS.md: раздел «Ветка retarget-tdl» → актуальные тулчейны/входы; nitra-доки где нужно
- [ ] N3.2 Отчёт-резюме + Deviations

## Правила-напоминания
- Пуш (kl/github) — только по явному решению пользователя.
- В nemerle потребители не пересобирают nemerle (только готовые дропы); баг компилятора → атом в nemerle с гейтами FC ×10 + сьют.
- `master`-ветки nitra/TDL не трогать; в `retarget-tdl` не носить FX/VSIX-частное.
- untracked в nitra (`AI/`, `diff.txt`) не трогать; HEAD nitra с `test` не сбрасывать без ff-проверки.
- Скретч/логи: `C:\RSDN\nemerle\.tmp\nitra-tdl\`.

## Журнал
- 2026-10-31: план составлен по разведке (макеты путей совпадают; nitra kl/retarget-nitra = dnlib-ветка, ff безопасен; TDL retarget-tdl уже Core-net5.0, не собирается из-за старого nitra Stage1). Исполнение стартует после завершения фазы 9 nemerle (разделение билд-ресурса).
