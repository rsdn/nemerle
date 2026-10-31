# Чек-лист: перевод nitra и TDL на nemerle@retarget-compiler

План: `nitra-tdl-retarget-plan.md`. Приёмка: **TDL собирается на dotnet 10, все тесты TDL зелёные**.
Порядок: строго один атом, один суб-агент, один билд в системе.

## Фаза N0 — Research
- [✅] N0.1 report-n0-1-nitra.md: цепочка Build.cmd→BootTasks BuildBoot (Boot1 FX-сид 2019-года)→Nitra-Stagt1.sln; consumer-API топа совместен as-is (tasks/namespace/Sdk-props/ncc-auto-remap) кроме: VS2017-пин во входах (обходится прямым msbuild + MSBUILDENABLEALLPROPERTYFUNCTIONS=1), ShiftBoot NccFile-список (ncc.exe/32/64 — блокер только для reseed). ПРОБА: BuildBoot зелёный; Stage1 за 6 мин — 3 ошибки в 2 проектах: Nitra.ClientServer.Server (resx-макро = FX-only, наш 7.5!) + VSIX-легаси (вне скоупа). Все три TDL-входа (Stage1/Grammars/LanguageCompiler) СОБРАНЫ. Атомизация N1.1-N1.9 в репорте
- [ ] N0.2 Инвентарь `kl/retarget-tdl`: TFMs (net5.0→net10 вопрос), вход Build.cmd vs реальная машина, контур тестов и baseline → report-n0-2-tdl.md
- [ ] N0.3 Уточнение атомизации N1/N2 по отчётам (правки этого чек-листа)

## Фаза N1 — nitra (ветка `retarget-nitra`, ff до `kl/retarget-nitra`)
- [ ] N1.1 Wiring: сборка nitra резолвит nemerle-дроп топа (пути/ncc-core/tasks/Sdk-props) — сухие прогоды/первые ошибки классифицированы
- [ ] N1.2 Бутстрап-цепочка nitra (Boot→Stage1) зелёная свежим nemerle
- [ ] N1.3 Перешивка boot-дропа nitra (если требуется N0.1) — отдельный коммит после зелёного прогона (правило nemerle 4.6)
- [ ] N1.4 Полный билд nitra (Stage-выходы + Grammars + LanguageCompiler) зелёный
- [ ] N1.5 Тесты nitra: baseline-норма N0.1, без регрессий

## Фаза N2 — TDL (ветка `retarget-tdl`)
- [ ] N2.1 Билд TDL против новой nitra/nemerle зелёный (TFM-политика по N0.2: «на dotnet 10»)
- [ ] N2.2 Тесты TDL: `Tdl.Tests` golden (все живые тулчейны) + `Tdl.Transformator.Tests` — **все зелёные** = финальная приёмка

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
