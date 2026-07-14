# SKILLS.md

Instruções para acelerar desempenho de agentes de IA usando este repositório.

---

## 1) Primeiro passo: use o `PROJECT_MAP.md`
- Leia **`.agents/PROJECT_MAP.md`** para entender onde está o código.
- Em seguida, escolha a seção do mapa que corresponde ao seu objetivo:

> Para reduzir gasto de tokens, use um cache local (abaixo) antes de refazer indexações/leitura repetitiva.

  - Rotas/servidor → `src/base/RALRoutes.pas`, `src/base/RALServer.pas`
  - Params/request/response → `src/base/RALParams.pas`, `src/base/RALRequest.pas`, `src/base/RALResponse.pas`
  - Multipart → `src/utils/RALMultipartCoder.pas`
  - Storage/serialization → `src/utils/RALStorage*.pas`
  - Compress/Cripto → `src/utils/RALCompress*.pas`, `src/utils/RALCripto*.pas`, `src/utils/RALStream.pas`
  - Engines de transporte → `src/engine/*`
  - DBModule/DBWare → `src/database/*`
  - Wizard → `src/wizard/*`

---

## 2) Evite “caçar” no repositório (reduz custo e tempo)
- Não comece por arquivos aleatórios.
- Sempre derive 2-3 unidades-alvo a partir do mapa.
- Se precisar localizar algo específico (ex.: “EncodeBody”, “DecodeBody”, “ApplyUpdatesRemote”), faça uma busca direcionada a partir do módulo.

---

## 2.1) Cache local de agente (evita reprocessar o repositório)
- Crie um arquivo **LOCAL_AGENT_CACHE.md** (na raiz do seu workspace local, junto com o arquivo `.agents/`).
- Esse arquivo é **somente local**: não committe e não envie em PR.
- Estrutura sugerida:
  - `# Cache - PascalRAL` 
  - `## Última atualização (timestamp)`
  - `## Mudanças recentes detectadas` (ex.: resumo de diff do que você alterou ou do que precisa revisar)
  - `## Index local` (lista curta de unidades/trechos relevantes com links ou caminhos)

## 3) Trate `compiled/` como lixo temporário

- O diretório **`compiled/`** contém artefatos gerados (PPU etc.).
- Não use `compiled/` como fonte da lógica: prefira sempre `src/`.

---

## 3.1) Gere exemplos seguindo o estilo do repo (evite suposições)
Quando for criar um exemplo mínimo, **não invente nomes de métodos/handlers**. Faça assim:
1. Leia o engine escolhido (ex.: `src/engine/synopse/RALSynopseServer.pas`) e confirme:
   - como instanciar o server (classe do engine)
   - como registrar rotas (ex.: `CreateRoute`, `AddRoute` ou semelhante)
2. Leia a assinatura do handler (ex.: `procedure X(ARequest: TRALRequest; AResponse: TRALResponse)`), e copie o padrão exato.
3. Só então escreva o exemplo no chat.

Exemplo de padrão de handler (deve ser confirmado no repo)
- `procedure pingReply(ARequest: TRALRequest; AResponse: TRALResponse);`
- dentro: **RECOMENDADO** `AResponse.Answer(HTTP_OK, 'pong', rctTEXTPLAIN);`
- alternativa válida: `AResponse.Answer(200, 'pong', 'text/plain');`

> Preferir o modo com constantes (`HTTP_OK` e `rctTEXTPLAIN`) tende a ser mais legível e pode reduzir tokens.




---

## 4) Performance: prefira leitura de funções “ponto único”
- Quando o objetivo for performance (latência/throughput), primeiro examine:
  - funções de parse/encode em `RALParams`, `RALRequest`, `RALResponse`
  - multipart e stream em `RALMultipartCoder` e `RALStream`
  - compressão/cripto em `RALCompress*` e `RALCripto*`
- Só depois desça para engines (`src/engine/*`) para entender overhead de transporte.

---

## 5) Performance: cuidado com multi-thread
- Procure por:
  - `ThreadSafe` em `src/utils/RALThreadSafe.pas`
  - implementações multithread nas engines/clients (quando existirem)
- Sempre verifique ownership/lifetime de objetos ao modificar lógica.

---

## 6) Mudanças seguras
- Antes de editar qualquer unit base (`src/base/*`), leia também:
  - unidades que consomem essas classes nos engines
  - unidades de utilitários que são chamadas recursivamente
- Evite alterar interfaces públicas sem checar impacto no resto do projeto.

---

## 7) Formato recomendado para propostas/PRs
Ao planejar mudanças, siga este formato:
- **Motivo** (bug/performance/feature)
- **Arquivos afetados**
- **Mudança exata** (função/método)
- **Risco** (compatibilidade Delphi/Lazarus, multi-thread)
- **Como validar** (testes manuais/compilação)

---

## 8) “Checklist” rápido (para agentes)
- [ ] Use `PROJECT_MAP.md`
- [ ] Comece por `src/` (não use `compiled/`)
- [ ] Identifique unidade/método-alvo
- [ ] Verifique chamadas e dependências
- [ ] Valide comportamento esperado

