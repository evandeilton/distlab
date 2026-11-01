# Histórico compartilhado — Claude e Sol/Codex

Este arquivo registra a continuidade do projeto a pedido de Evandeilton.
Leia também `AGENTS.md`, `CLAUDE.md` e `PLAN.md`. Preserve as entradas e
acrescente uma nova a cada conjunto de trabalho, inclusive trabalho parcial.

## Estado na entrada de Sol/Codex — 2026-11-01

- Branch: `feat/simulation-lab`.
- HEAD de referência: `21b1fc5` — Refine the Simulation lab: lattice bins,
  notes and phone layout.
- Diretório de trabalho inicialmente limpo.
- `main` local e referência local `origin/main`: `d00dbd7`, merge do PR #4.
  O estado remoto ao vivo não foi consultado.
- Lotes 1–4 constam como integrados no histórico e no plano.
- Lote 5 tem quatro commits além da `main`: `3aed65d`, `0002bb2`,
  `0bc3caf` e `21b1fc5`. Incluem motor, interface, NEWS e refinamentos
  da Simulação. O plano ainda o marcava como próximo.
- Fit lab permanece uma tela provisória em `R/run_app.R`; lotes 6–9
  continuam pendentes.

### Análise e validação inicial — Sol/Codex

Inspecionados o histórico Git, o plano, a descrição do pacote, NEWS, a
composição do app, o motor e o módulo de Simulação, os testes e o workflow
de CI. Nenhum código funcional foi alterado nessa análise.

Comando executado: `Rscript -e 'devtools::test(reporter = "summary")'`.
Resultado: execução concluída com código 0, sem falhas; um aviso e um teste
pulado:

- Aviso: cache de CSS/Sass em `~/.cache/R/sass/` sem permissão de escrita
  no ambiente restrito desta sessão.
- Teste de navegador `test-app-e2e.R`: pulado porque exige o pacote
  instalado, em vez de carregado por devtools.

Log temporário desta execução: `/tmp/distlab-takeover-tests.log`.
Ainda não foram feitos R CMD check, revisão visual em navegador, consulta
ao CI remoto, push, criação de PR ou merge nesta participação.

### Acordo de colaboração e documentação — Sol/Codex

Solicitação do usuário: declarar a colaboração ao Claude, registrar tudo o
que for feito e usar a identidade/credenciais do proprietário sem coautoria
de assistentes.

Arquivos deste conjunto de trabalho:

- `AGENTS.md`: instruções persistentes para registro e identidade Git.
- `CLAUDE.md`: aviso dirigido ao Claude e referências de continuidade.
- `COLLABORATION.md`: este histórico e o diagnóstico inicial.
- `PLAN.md`: atualização local do estado do lote 5 e do próximo passo
  (arquivo já ignorado pelo Git).
- `.Rbuildignore`: exclusão dos três documentos de colaboração do pacote R.

A configuração Git existente já informa José Evandeilton Lopes e
evandeilton@gmail.com; não foi alterada. A autenticação do GitHub não foi
consultada nem modificada. Nenhum commit foi criado neste conjunto de trabalho.
Mudanças restritas à documentação e à lista de exclusão do build.
Validação concluída: `git diff --check` sem erros, inspeção do diff e
checagem em R confirmando que os três novos documentos são excluídos pelo
`.Rbuildignore`. A suíte R não foi repetida para estas mudanças documentais.

Próximo passo: revisar o diff do lote 5, validar visualmente a Simulação,
executar os checks do pacote e conferir o CI/PR antes de encerrar o lote.
Depois, seguir para o lote 6 (Fit lab). Não iniciar trabalho concorrente nos
mesmos arquivos sem conferir o Git e este registro.

### Atualização dos lotes pendentes — 2026-11-01 — Sol/Codex

Solicitação: atualizar `PLAN.md` e mostrar quais lotes ainda faltam.

- Conferidos o HEAD, o diretório de trabalho, as exportações em `NAMESPACE`
  e a ausência das implementações de Fit, relatório e novos cartões.
- `PLAN.md`: desmembrados os lotes 6–9 na tabela de estado; incluída uma
  lista de entregas e critérios de fechamento para os lotes 5–9.
- Diferenciado o fechamento da Simulação dos quatro lotes restantes de
  implementação. O lote 9 tem controles de cópia já existentes, mas ainda
  precisa das demais entregas. Mantida a ordem do plano original.
- Corrigido o item antigo que ainda tratava marca e logo como não implementados.
- Registrada a conferência final do escopo/API do MVP antes da versão 0.1.0,
  preservando bootstrap, catálogos T2/T3 e shinylive nas versões posteriores.
- Arquivos alterados nesta etapa: `PLAN.md` e este histórico. Sem mudanças
  funcionais, commits, push ou consulta ao GitHub remoto.
- Validação: leitura do plano, conferência das linhas alteradas e checagem
  de whitespace; testes R não se aplicam a esta atualização documental.

Próximo passo permanece o fechamento do lote 5, seguido pelo lote 6 (Fit lab).

### Início do lote 6 — 2026-11-01 — Sol/Codex, orquestrador

O proprietário autorizou iniciar o lote 6 agora, com três agentes experts e
um orquestrador, preservando a qualidade e o estilo. O fechamento do lote 5
continua pendente; não foi feito merge implícito nem descartado seu trabalho.

- Criada a branch local `codex/fit-lab`, baseada em `21b1fc5`, preservando
  as alterações documentais já existentes. Sem commits/push.
- Agente `fit_engine`: `R/fit.R` e testes do motor; MLE, multistart,
  incertezas, parâmetros fixos, ranking e código reproduzível.
- Agente `fit_data_diag`: `R/fit_data.R`, `R/fit_diagnostics.R` e testes;
  importação, frequências/classes, descritivas, candidatas, gráficos e GOF.
- Agente `fit_ui`: `R/mod_fit.R`, traduções `fit.*`, CSS e testes do módulo;
  entrada de dados, ajuste explícito, diagnósticos e exportação de tabelas/código.
- Sol/Codex: contratos entre componentes, integração no app, dependências,
  documentação da API, revisão numérica/UI e validação final.

Decisões contratuais: dados de frequência não serão expandidos sem limite;
verossimilhança agrupada usará probabilidades de intervalos; densidades e
massas não terão delta AIC/pesos comparados entre si. Parâmetros inteiros de
dimensão exigirão fixação explícita quando não estimados. GOF com parâmetros
estimados será rotulado descritivo; bootstrap permanece v1.

Referências primárias consultadas pelo orquestrador:
- https://stat.ethz.ch/R-manual/R-devel/library/stats/html/AIC.html
- https://stat.ethz.ch/R-manual/R-devel/library/stats/html/ks.test.html
- https://cran.r-project.org/web/packages/goftest/goftest.pdf

Dependências `data.table`, `numDeriv` e `goftest` já estão instaladas neste
ambiente e passam a Imports do pacote. Validações e conclusão serão
acrescentadas após a integração; esta entrada registra trabalho em andamento.

### Integração e revisão holística do lote 6 — Sol/Codex

O proprietário autorizou Terra nas delegações seguintes, mantendo o consenso
com Sol, e pediu leitura holística do trabalho do Claude. Foi acrescentado
`docs/architecture.md`: histórico de todos os lotes, mapa das camadas,
relação entre 21 entradas/28 arestas, invariantes matemáticas, estado da UI,
ativos locais, testes e distinção entre implementação e intenções do plano.

Entregas dos especialistas integradas pelo orquestrador:

- `R/fit.R`: `dl_fit()`/`print.dl_fit`, quatro partidas determinísticas de
  `nlminb`, escala log/logit, Hessiana `numDeriv`, parâmetros fixos,
  estimativas/SE/IC, ranking por medida de verossimilhança e script com os
  dados necessários para reproduzir os resultados.
- `R/fit_data.R`: `dl_fit_data()`, `dl_fit_describe()` e
  `dl_fit_candidates()`; compressão de frequências, classes `(lower, upper]`,
  CSV/TSV/vírgula decimal, resumos exatos para frequências e aproximados
  para classes. Classes abertas preservadas sem inventar pontos médios.
- `R/fit_diagnostics.R`: probabilidades das mesmas classes nos gráficos
  agrupados; ECDF/CDF, Q-Q pontual condicional e P-P para dados individuais;
  KS/AD/CvM e chi-square descritivos. Gaps e caudas permanecem na partição,
  classes esperadas são agregadas e graus de liberdade descontam parâmetros.
- `R/mod_fit.R`, dicionários `fit.*`, CSS: entrada explícita de dados,
  candidatas e parâmetros estruturais fixos, estimação por botão,
  preservação dos resultados ao trocar idioma/tema, avisos localizados,
  CSV de dados/tabelas, código R e navegação ao Explorer.
- `R/run_app.R`: Fit substitui a tela provisória e participa do roteamento.
- `DESCRIPTION`: novos Imports `data.table`, `datasets`, `goftest`,
  `numDeriv`; Suggests `MASS` para crosscheck independente dos testes.
- `NAMESPACE` e `man/*.Rd` regenerados com roxygen; README e NEWS atualizados.

Regras estatísticas e limitações explícitas:

- Binomial/Beta-binomial exigem `size` fixo; Hipergeométrica exige `m,n,k`
  fixos; uniformes agrupadas exigem limites fixos. Student t segue a família
  do registro, com localização zero/escala um e somente `df` livre.
- Extremos de uniformes e localização de Laplace são não regulares;
  incerteza Wald indisponível nos casos correspondentes, com aviso.
- Dados constantes, limites sem MLE interior e falta de sobredispersão da
  Negativa binomial produzem explicações; não convergência não ganha pesos.
- GOF/QQ não recebem calibração de bootstrap neste lote. Classes agrupadas
  não geram observações individuais artificiais para Q-Q/P-P.

Revisão independente Terra (`fit_review_terra`), com consenso/correção Sol:

- Encontrou P1 de falsa convergência em classes pouco informativas, como
  Normal livre em uma classe `(-1,1]` e duas metades de R. A saturação da CDF
  podia simular um MLE finito ou identificação inexistente.
- Sol acrescentou `fit_grouped_check()`: posto das probabilidades da
  partição e curvatura em coordenadas sem unidade, incluindo limites dos
  gaps/caudas. Partições planas ou numericamente instáveis viram falhas
  explícitas sem pesos. Regressões incluem Student t com corte zero,
  parâmetros parcialmente fixos e classes disjuntas com MLE regular.
- Na integração, Sol corrigiu o seletor de diagnósticos quando todos os
  candidatos falham, traduziu os avisos e preservou rótulos de parâmetros
  fixos/estimados. A primeira suíte completa revelou esse estado vazio;
  as regressões direcionadas passaram após a correção.
- A página longa foi organizada nas abas Dados, Ajustes e ranking,
  Diagnósticos e Exportação, seguindo as outras páginas. Resumo em tiles,
  demais medidas/requisitos em detalhes expansíveis; modelo inicial de
  diagnóstico segue a ordem do ranking.
- Chrome confirmou desktop pt-BR, idioma/tema sem reestimação, downloads
  e execução do script baixado, além de largura móvel sem overflow global.
  Identificou também truncamento dos ajustes ao abrir no Explorer.
- Sol corrigiu `R/constraints.R` para incluir parâmetros válidos fora das
  janelas didáticas e `R/params.R` para oferecer grade de até seis casas em
  parâmetros importados fora da grade original. O objeto de ajuste e seus
  arquivos exportados mantêm a precisão completa; sliders são controles de
  exploração com precisão de interface. Incluídas regressões unitárias e
  de navegador para o transporte de modelos ajustados.

Os especialistas entregaram arquivos e validações direcionadas; os agentes
de motor/UI e o revisor Terra foram interrompidos por limite de uso nas
rodadas finais. Sol preservou as entregas e assumiu as correções e a
validação final. Não houve commits, push ou operação de conta GitHub.

Em execução: suíte completa atualizada, R CMD check do pacote e validação
final no Chrome. Os resultados definitivos serão registrados a seguir.

### Fechamento da implementação local do lote 6 — 2026-11-01 — consenso Sol

Correções finais adicionais:

- `param_sliders()` agora aguarda o eco dos valores enviados por navegação.
  O teste de ponta a ponta mostrou que valores de uma visita antiga ao
  Explorer podiam substituir o ajuste quando se trocava de família.
  A regressão verifica o estado antigo, o eco e uma edição manual posterior.
- Diagnósticos sem `id` explícito ignoram famílias que falharam em grupos
  anteriores do ranking, escolhendo um modelo efetivamente ajustado.
- Qualificados `utils::head()`/`utils::tail()` após o check apontar imports
  implícitos. A validação posterior ficou sem notas.
- Teste de navegador ampliado para Fit: ações reais, espera por renderização
  das abas, idioma mantendo código, gráficos e transporte de parâmetros.

Validação concluída:

- Suíte completa `devtools::test(reporter = "summary", stop_on_failure = TRUE)`:
  17.893 expectativas aprovadas, 0 falhas, 0 erros e 0 avisos; um teste
  pulado porque depende do pacote instalado. Registro em
  `/tmp/distlab-fit-final-tests.log` e resumo RDS correspondente.
- Regressões adicionais de diagnóstico e de roteamento após essa suíte;
  os módulos que compartilham parâmetros também foram exercitados novamente
  no check final do pacote. Fontes novos conferidos quanto a UTF-8/whitespace,
  e `git diff --check` sem erros.
- Pacote instalado em `/tmp/distlab-fit-library`. Teste real de navegador
  `test-app-e2e.R` aprovado com 25 verificações, sem falhas/avisos/skip.
  Registro: `/tmp/distlab-fit-e2e-final.log`.
- QA Chrome adicional: desktop pt-BR, inglês/tema escuro, tela 390 px sem
  overflow global, CSV e script R baixados, execução do script reproduzindo
  o ranking, parâmetros transportados para o Explorer dentro e fora das
  janelas iniciais. Capturas e exports temporários em `/tmp/distlab-fit-qa/`.
- R CMD check completo final, com todos os testes do pacote, terminou em
  **0 erros, 0 avisos e 0 notas**. Registro:
  `/tmp/distlab-fit-check-delivery.log`; relatório em
  `/tmp/distlab-fit-check-delivery/distlab.Rcheck/00check.log`.
- Prévia local da árvore atual iniciada em `http://localhost:4322`.
  A porta 4321 já tinha um processo; ele foi preservado. Log da nova prévia:
  `/tmp/distlab-fit-preview.log`.

`PLAN.md` atualizado: lote 6 implementado/validado localmente; lotes 7–9
continuam pendentes. A dependência do lote 5 permanece explícita. Não houve
commit, push, PR, merge ou mudança de credenciais. GitHub remoto/CI não
foram consultados. Antes da entrega remota, revisar o diff e ordenar a
integração dos lotes 5/6; eventuais commits devem usar somente a identidade
do proprietário, sem coautoria de assistentes.

### Preparação da nova PR — 2026-11-01 — Sol

O proprietário solicitou comitar as mudanças e criar uma nova PR.

- Identidade efetiva de autor e committer conferida: José Evandeilton Lopes
  <evandeilton@gmail.com>. Autenticação existente do GitHub confirmada na
  conta `evandeilton`; nenhuma conta ou credencial foi alterada.
- Referências de `origin` atualizadas. GitHub confirma PRs #1–4 integradas,
  nenhuma PR aberta e `main` ainda na base do lote 4.
- A nova PR será contra `main` e incluirá Simulação + Fit lab, preservando
  os quatro commits existentes do lote 5. Nenhuma coautoria de assistente
  será adicionada aos novos commits; commits anteriores não serão reescritos.
- `PLAN.md` passa a ser versionado: removida somente sua exclusão no
  `.gitignore`. Continua excluído do build R. Assim, plano, instruções e
  histórico de continuidade estarão disponíveis também no repositório.
- Verificações finais: diff sem erros de whitespace; validação funcional
  já concluída e registrada acima (suíte, navegador e R CMD check limpo).
  Esta preparação altera somente documentação e regras de versionamento.

O hash do commit e o endereço da PR serão registrados após a publicação.
