/// User-facing texts of the migrator core, kept in one place for translation.
unit RALMig.Mensagens;

{$mode ObjFPC}{$H+}

{ Como em RALInst.Mensagens, o prefixo diz o peso da mensagem:
    em = erro: algo nao foi feito
    wm = aviso: foi feito, mas o usuario precisa saber de algo
    cm = texto comum: rotulos, relatorio e log

  O texto original e o portugues; as traducoes ficam em
  languages/ralmigrador.<idioma>.po (embutidas no executavel). Nao troque os
  curingas '%': a traducao precisa dos mesmos, na mesma ordem. Cada etapa do
  migrador acrescenta a sua secao aqui. }

interface

resourcestring
  // RALMig.Copia
  emCopiaSemPasta = 'A pasta do projeto não existe: %s';
  emCopiaSemArquivos = 'Não há arquivos para guardar na cópia de segurança.';
  emCopiaCaminhoInvalido = 'Caminho fora da pasta do projeto, recusado: %s';
  emCopiaNaoCriada = 'Não foi possível criar a cópia de segurança %s: %s';
  emCopiaNaoExiste = 'A cópia de segurança não existe: %s';
  emCopiaIlegivel = 'A cópia de segurança %s não pôde ser lida: %s';
  emCopiaSemManifesto = 'A cópia de segurança %s não tem a lista de arquivos ' +
    '(_manifesto.txt): não foi criada pelo migrador.';
  emCopiaNaoRestaurada = 'Não foi possível restaurar %s: %s';
  emCopiaDiferente = 'Depois de restaurar, %s não ficou igual ao original.';
  emCopiaSemRegistro = 'Não foi possível registrar o estado dos arquivos depois ' +
    'da migração: %s';
  wmCopiaMudadoDepois = 'Este arquivo foi alterado depois da migração e a ' +
    'restauração o sobrescreveu: %s';
  wmGitComMudancas = 'A pasta está num repositório git com mudanças ainda não ' +
    'commitadas (%s). Faça o commit antes de migrar para poder comparar e ' +
    'desfazer pelo git.';

  // RALMig.Previa
  cmPreviaSemMudanca = 'Sem mudanças.';
  cmPreviaResumo = '+%d linha(s) acrescentada(s), -%d linha(s) tirada(s)';

implementation

end.
