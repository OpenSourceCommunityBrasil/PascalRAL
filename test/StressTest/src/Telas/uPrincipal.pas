unit uPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.DateUtils, System.Rtti,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.Edit,
  FMX.Layouts, FMX.Objects, FMX.ListBox, FMX.TabControl, FMX.Grid.Style,
  FMX.Grid,
  REST.Types,

  RALConsts,

  uResultado, TestUnit, DAOBase, uRESTDAO

    ;

type
  TRESTClientKind = (rckREST, rckRALIndy, rckRALSynopse);

  TfPrincipal = class(TForm)
    Layout1: TLayout;
    Image1: TImage;
    lVersao: TLabel;
    TabControl1: TTabControl;
    tiSimples: TTabItem;
    tiAvancado: TTabItem;
    FlowLayout1: TFlowLayout;
    eServidor: TEdit;
    Label1: TLabel;
    ePorta: TEdit;
    Label2: TLabel;
    eEndpoint: TEdit;
    Label3: TLabel;
    FlowLayout2: TFlowLayout;
    cbAutenticacao: TComboBox;
    Label4: TLabel;
    eUsuario: TEdit;
    Label5: TLabel;
    eSenha: TEdit;
    Label6: TLabel;
    FlowLayout3: TFlowLayout;
    Button1: TButton;
    Button2: TButton;
    Layout2: TLayout;
    StringGrid1: TStringGrid;
    FlowLayout5: TFlowLayout;
    cbMetodoAv: TComboBox;
    Label10: TLabel;
    eUsuarioAv: TEdit;
    Label11: TLabel;
    eSenhaAv: TEdit;
    Label12: TLabel;
    FlowLayout4: TFlowLayout;
    eServidorAv: TEdit;
    Label7: TLabel;
    ePortaAv: TEdit;
    Label8: TLabel;
    eEndpointAv: TEdit;
    Label9: TLabel;
    bAdicionar: TButton;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    FlowLayout6: TFlowLayout;
    Button5: TButton;
    Layout3: TLayout;
    Layout4: TLayout;
    gbVerbos: TGroupBox;
    cbGET: TCheckBox;
    cbPOST: TCheckBox;
    cbPUT: TCheckBox;
    cbPATCH: TCheckBox;
    cbDELETE: TCheckBox;
    gbClientes: TGroupBox;
    cbRESTNativo: TCheckBox;
    bRemover: TButton;
    GroupBox1: TGroupBox;
    rbSequencial: TRadioButton;
    rbParalelo: TRadioButton;
    GroupBox2: TGroupBox;
    eRequisicoes: TEdit;
    Label13: TLabel;
    eConcorrentes: TEdit;
    Label14: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    procedure IniciarClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Rectangle1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Rectangle2Click(Sender: TObject);
    procedure bAdicionarClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure bRemoverClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    TestObjects: array of TTestObject;
    REST: TRESTDAO;
    inicio, fim: Double;
    pass, fail: integer;
    TelaResultado: TFResultado;
    procedure DefinirParametrosTeste(aClient: TRESTClientKind; aMemo: TMemo);
    procedure LimparObjetosTeste;
    procedure IniciaTestes;
    procedure EncerraTeste;
    procedure Testar;
    procedure TesteRESTRequest(aClient: TRESTDAO);
    procedure TesteEndpointREST(aEndpoint: string; metodo: TTestRequestMethod;
      count: integer; aClient: TRESTDAO);
  public
    { Public declarations }
  end;

var
  fPrincipal: TfPrincipal;

implementation

{$R *.fmx}

procedure TfPrincipal.bAdicionarClick(Sender: TObject);
begin
  with StringGrid1 do
  begin
    RowCount := RowCount + 1;
    Cells[0, pred(RowCount)] := eServidorAv.Text;
    Cells[1, pred(RowCount)] := ePortaAv.Text;
    Cells[2, pred(RowCount)] := eEndpointAv.Text;
    if cbMetodoAv.ItemIndex <> -1 then
    begin
      Cells[3, pred(RowCount)] := cbMetodoAv.Selected.Text;
      Cells[4, pred(RowCount)] := eUsuarioAv.Text;
      Cells[5, pred(RowCount)] := eSenhaAv.Text;
    end;
  end;
end;

procedure TfPrincipal.bRemoverClick(Sender: TObject);
begin
  if StringGrid1.RowCount > 0 then
    StringGrid1.RowCount := StringGrid1.RowCount - 1;
end;

procedure TfPrincipal.Button2Click(Sender: TObject);
begin
  if not Assigned(FResultado) then
    Application.CreateForm(TFResultado, FResultado);
  FResultado.Show;
  inicio := now;

  FResultado.LogMessage('Teste de 1000 requests sequenciais iniciados �s ' +
    TimeToStr(inicio));
  REST := TRESTDAO.Create(eServidor.Text, ePorta.Text);
  if (cbAutenticacao.ItemIndex = 1) and
    ((eUsuario.Text <> EmptyStr) and (eSenha.Text <> EmptyStr)) then
  begin
    REST.SetBasicAuth(eUsuario.Text, eSenha.Text);
  end;

  TThread.CreateAnonymousThread(
    procedure
    begin
      pass := 0;
      fail := 0;
      FResultado.LogMessage('Iniciando testes com REST Nativos...');
      TesteEndpointREST(eEndpoint.Text, rtmGET, 1000, REST);

      EncerraTeste;

      REST.Free;
    end).Start;
end;

procedure TfPrincipal.Button5Click(Sender: TObject);
begin
  FResultado.Show;
  // TThread.CreateAnonymousThread(IniciaTestes).Start;
  TThread.CreateAnonymousThread(Testar).Start;
end;

procedure TfPrincipal.DefinirParametrosTeste(aClient: TRESTClientKind;
aMemo: TMemo);
var
  dummytest: TTestObject;
  I: integer;
  RESTClient: TDAOBase;
begin
  for I := 0 to pred(StringGrid1.RowCount) do
  begin
    case aClient of
      rckREST:
        RESTClient := TRESTDAO.Create(StringGrid1.Cells[0, I],
          StringGrid1.Cells[1, I]);
      rckRALIndy:
        ;
      rckRALSynopse:
        ;
    end;

    dummytest := TTestObject.Create(RESTClient, aMemo);
    with dummytest do
    begin
      Server := StringGrid1.Cells[0, I];
      Port := StrToIntDef(StringGrid1.Cells[1, I], 0);
      Endpoint := StringGrid1.Cells[2, I];
      if StringGrid1.Cells[3, I].Contains('Basic') then
      begin
        AuthType := atBasic;
        AuthUser := StringGrid1.Cells[4, I];
        AuthPassword := StringGrid1.Cells[5, I];
      end
      else
        AuthType := atNone;

      Methods := [];
      if cbGET.IsChecked then
        Methods := Methods + [rtmGET];
      if cbPOST.IsChecked then
        Methods := Methods + [rtmPOST];
      if cbPUT.IsChecked then
        Methods := Methods + [rtmPUT];
      if cbPATCH.IsChecked then
        Methods := Methods + [rtmPATCH];
      if cbDELETE.IsChecked then
        Methods := Methods + [rtmDELETE];

      if rbSequencial.IsChecked then
        TestType := ttSequencial
      else if rbParalelo.IsChecked then
      begin
        TestType := ttConcorrente;
        CcRequestCount := StrToIntDef(eConcorrentes.Text, 0);
      end;
      RequestCount := StrToIntDef(eRequisicoes.Text, 0);
    end;
    SetLength(TestObjects, Length(TestObjects) + 1);
    TestObjects[High(TestObjects)] := dummytest;
  end;
end;

procedure TfPrincipal.EncerraTeste;
begin
  fim := now;
  FResultado.LogMessage('Testes finalizados ap�s ' +
    FormatDateTime('hh:nn:ss:zzz', (fim - inicio)) + ' (hor:min:seg:mil)');
  FResultado.LogMessage(Format(' - Total: %d, Sucesso: %d, Falha: %d',
    [pass + fail, pass, fail]));
  FResultado.LogMessage('=======================================');
  LimparObjetosTeste;
end;

procedure TfPrincipal.FormCreate(Sender: TObject);
begin
  lVersao.Text := Format('Vers�o componentes: %s', [RALVERSION]);
  StringGrid1.RowCount := 0;
  FResultado := nil;
  Application.CreateForm(TFResultado, FResultado);
end;

procedure TfPrincipal.FormDestroy(Sender: TObject);
begin
  try
    FResultado.Free; // Porque? Porque o assigned n�o funciona
  except // basta ignorar o erro ou rodar em modo release
  end;
end;

procedure TfPrincipal.IniciarClick(Sender: TObject);
begin
  if not Assigned(FResultado) then
    Application.CreateForm(TFResultado, FResultado);
  FResultado.Show;
  inicio := now;

  FResultado.LogMessage('Testes sequenciais iniciados �s ' + TimeToStr(inicio));
  REST := TRESTDAO.Create(eServidor.Text, ePorta.Text);
  if (cbAutenticacao.ItemIndex = 1) and
    ((eUsuario.Text <> EmptyStr) and (eSenha.Text <> EmptyStr)) then
  begin
    REST.SetBasicAuth(eUsuario.Text, eSenha.Text);
  end;
  TThread.CreateAnonymousThread(
    procedure
    begin
      TesteRESTRequest(REST);

      EncerraTeste;
    end).Start;
end;

procedure TfPrincipal.IniciaTestes;
var
  I: integer;
  RESTClient: TRESTDAO;
begin
  inicio := now;
  FResultado.LogMessage('Testes sequenciais iniciados �s ' + TimeToStr(inicio));
  FResultado.LogMessage('-------------------------------------');
  for I := 0 to pred(StringGrid1.RowCount) do
  begin
    if cbRESTNativo.IsChecked then
    begin
      RESTClient := TRESTDAO.Create(StringGrid1.Cells[0, I],
        StringGrid1.Cells[1, I]);
      if (cbMetodoAv.ItemIndex = 1) and
        ((eUsuarioAv.Text <> EmptyStr) and (eSenhaAv.Text <> EmptyStr)) then
        RESTClient.SetBasicAuth(eUsuarioAv.Text, eSenhaAv.Text);
    end;

    if cbGET.IsChecked then
    begin
      if cbRESTNativo.IsChecked then
      begin
        FResultado.LogMessage
          (Format('Testando servidor %s:%s com REST Nativos...',
          [StringGrid1.Cells[0, I], StringGrid1.Cells[1, I]]));
        FResultado.LogMessage('Testando verbo GET...');
        TesteEndpointREST(StringGrid1.Cells[2, I], rtmGET,
          eRequisicoes.Text.ToInteger, RESTClient);
      end;
    end;

    if cbPOST.IsChecked then
    begin
      if cbRESTNativo.IsChecked then
      begin
        FResultado.LogMessage
          (Format('Testando servidor %s:%s com REST Nativos...',
          [StringGrid1.Cells[0, I], StringGrid1.Cells[1, I]]));
        FResultado.LogMessage('Testando verbo POST...');
        TesteEndpointREST(StringGrid1.Cells[2, I], rtmPOST,
          eRequisicoes.Text.ToInteger, RESTClient);
      end;
    end;

    if cbPUT.IsChecked then
    begin
      if cbRESTNativo.IsChecked then
      begin
        FResultado.LogMessage
          (Format('Testando servidor %s:%s com REST Nativos...',
          [StringGrid1.Cells[0, I], StringGrid1.Cells[1, I]]));
        FResultado.LogMessage('Testando verbo PUT...');
        TesteEndpointREST(StringGrid1.Cells[2, I], rtmPUT,
          eRequisicoes.Text.ToInteger, RESTClient);
      end;
    end;

    if cbPATCH.IsChecked then
    begin
      if cbRESTNativo.IsChecked then
      begin
        FResultado.LogMessage
          (Format('Testando servidor %s:%s com REST Nativos...',
          [StringGrid1.Cells[0, I], StringGrid1.Cells[1, I]]));
        FResultado.LogMessage('Testando verbo PATCH...');
        TesteEndpointREST(StringGrid1.Cells[2, I], rtmPATCH,
          eRequisicoes.Text.ToInteger, RESTClient);
      end;
    end;

    if cbDELETE.IsChecked then
    begin
      if cbRESTNativo.IsChecked then
      begin
        FResultado.LogMessage
          (Format('Testando servidor %s:%s com REST Nativos...',
          [StringGrid1.Cells[0, I], StringGrid1.Cells[1, I]]));
        FResultado.LogMessage('Testando verbo DELETE...');
        TesteEndpointREST(StringGrid1.Cells[2, I], rtmDELETE,
          eRequisicoes.Text.ToInteger, RESTClient);
      end;
    end;

    if Assigned(RESTClient) then
      RESTClient.Free;
  end;
  EncerraTeste;
end;

procedure TfPrincipal.LimparObjetosTeste;
var
  I: integer;
begin
  if Length(TestObjects) > 0 then
    for I := pred(Length(TestObjects)) downto 0 do
      FreeAndNil(TestObjects[I]);
  SetLength(TestObjects, 0);
end;

procedure TfPrincipal.Rectangle1MouseDown(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: Single);
begin
  StartWindowDrag;
end;

procedure TfPrincipal.Rectangle2Click(Sender: TObject);
begin
  Self.Close;
end;

procedure TfPrincipal.Testar;
var
  I: integer;
  dummytest: TTestObject;
  RESTClient: TRESTDAO;
  // RALIndyClient: TRALIndyClient;
  // RALSynopseClient: TRALSynopseClient;
begin
  LimparObjetosTeste;

  if not Assigned(FResultado) then
    Application.CreateForm(TFResultado, FResultado);
  try
    if StringGrid1.RowCount > 0 then
    begin
      if cbRESTNativo.IsChecked then
      begin
        DefinirParametrosTeste(rckREST, FResultado.Memo1);
      end;
    end;

    FResultado.Show;
    for I := 0 to pred(Length(TestObjects)) do
      TestObjects[I].Test;
  finally
    if Assigned(RESTClient) then
      FreeAndNil(RESTClient);
  end;

end;

procedure TfPrincipal.TesteEndpointREST(aEndpoint: string;
metodo: TTestRequestMethod; count: integer; aClient: TRESTDAO);
var
  I: integer;
  ini, fim: Double;
  erro: string;
begin
  FResultado.LogMessage('Testando %d requisi��es...', [count]);
  ini := now;
  for I := 0 to count do
    if not aClient.TesteEndpoint(aEndpoint, metodo, erro) then
    begin
      FResultado.LogMessage('%s ap�s %d requisi��es', [erro, I]);
      inc(fail);
      break;
    end;
  fim := now;
  inc(pass);

  FResultado.LogMessage(' - finalizado ap�s %s (min:seg:mil)',
    [FormatDateTime('nn:ss:zzz', (fim - ini))]);
  FResultado.LogMessage('=======================================');
end;

procedure TfPrincipal.TesteRESTRequest(aClient: TRESTDAO);
var
  erro: string;
begin
  pass := 0;
  fail := 0;
  FResultado.LogMessage('Realizando testes de Requisi��o com REST nativos...');
  if (eServidor.Text = EmptyStr) or (ePorta.Text = EmptyStr) then
  begin
    FResultado.LogMessage('Erro: Configura��es de servidor ou porta inv�lidas');
    exit;
  end
  else
  begin
    if not aClient.TesteEndpoint(eEndpoint.Text, rtmGET, erro) then
    begin
      FResultado.LogMessage(Format('Teste %s', [erro]));
      inc(fail);
    end
    else
    begin
      FResultado.LogMessage('M�todo GET dispon�vel');
      TesteEndpointREST(eEndpoint.Text, rtmGET, 100, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmGET, 1000, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmGET, 10000, aClient);

      FResultado.LogMessage('Teste GET conclu�do');
    end;

    if not aClient.TesteEndpoint(eEndpoint.Text, rtmPOST, erro) then
    begin
      FResultado.LogMessage(Format('Teste %s', [erro]));
      inc(fail);
    end
    else
    begin
      FResultado.LogMessage('M�todo POST dispon�vel');
      TesteEndpointREST(eEndpoint.Text, rtmPOST, 100, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmPOST, 1000, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmPOST, 10000, aClient);

      FResultado.LogMessage('Teste POST conclu�do');
    end;

    if not aClient.TesteEndpoint(eEndpoint.Text, rtmPUT, erro) then
    begin
      FResultado.LogMessage(Format('Teste %s', [erro]));
      inc(fail);
    end
    else
    begin
      FResultado.LogMessage('M�todo PUT dispon�vel');
      TesteEndpointREST(eEndpoint.Text, rtmPUT, 100, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmPUT, 1000, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmPUT, 10000, aClient);

      FResultado.LogMessage('Teste PUT conclu�do');
    end;

    if not aClient.TesteEndpoint(eEndpoint.Text, rtmPATCH, erro) then
    begin
      FResultado.LogMessage(Format('Teste %s', [erro]));
      inc(fail);
    end
    else
    begin
      FResultado.LogMessage('M�todo PATCH dispon�vel');
      TesteEndpointREST(eEndpoint.Text, rtmPATCH, 100, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmPATCH, 1000, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmPATCH, 10000, aClient);

      FResultado.LogMessage('Teste PATCH conclu�do');
    end;

    if not aClient.TesteEndpoint(eEndpoint.Text, rtmDELETE, erro) then
    begin
      FResultado.LogMessage(Format('Teste %s', [erro]));
      inc(fail);
    end
    else
    begin
      FResultado.LogMessage('M�todo DELETE dispon�vel');
      TesteEndpointREST(eEndpoint.Text, rtmDELETE, 100, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmDELETE, 1000, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmDELETE, 10000, aClient);

      FResultado.LogMessage('Teste DELETE conclu�do');
    end;
  end;
  FResultado.LogMessage('Fim de testes de Requisi��o com REST nativos...');
  FResultado.LogMessage(Format('Testes realizados: %d, Sucesso: %d, Falhas: %d',
    [pass + fail, pass, fail]));
end;

end.
