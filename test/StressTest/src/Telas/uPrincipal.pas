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

  uResultado, DAOBase, uRESTDAO

    ;

type
  TTestType = (ttSequencial, ttConcorrente);
  TAuthType = (atNone, atBasic);

  TTestObject = class
    FServer: string;
    FPort: integer;
    FEndpoint: string;
    FMethods: Set of TTestRequestMethod;
    FTestType: TTestType;
    FRequestCount: integer;
    FCcRequestCount: integer;
    FAuthType: TAuthType;
    FAuthUser: string;
    FAuthPassword: string;
  end;

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
    procedure DefinirParametrosTeste;
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

  FResultado.LogMessage('Teste de 1000 requests sequenciais iniciados às ' +
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
  TThread.CreateAnonymousThread(IniciaTestes).Start;
end;

procedure TfPrincipal.DefinirParametrosTeste;
var
  I: integer;
  dummytest: TTestObject;
begin
  LimparObjetosTeste;

  if StringGrid1.RowCount = 0 then
  begin
    dummytest := TTestObject.Create;
    with dummytest do
    begin
      FServer := eServidor.Text;
      FPort := StrToIntDef(ePorta.Text, 0);
      FEndpoint := eEndpoint.Text;
      FAuthType := TAuthType(cbAutenticacao.ItemIndex);
      if FAuthType <> atNone then
      begin
        FAuthUser := eUsuario.Text;
        FAuthPassword := eSenha.Text;
      end;
      FMethods := [rtmGET, rtmPOST, rtmPUT, rtmPATCH, rtmDELETE];
      FTestType := ttSequencial;
      FRequestCount := 1000;
    end;

    SetLength(TestObjects, 1);
    TestObjects[0] := dummytest;
  end
  else
  begin
    for I := 0 to pred(StringGrid1.RowCount) do
    begin
      dummytest := TTestObject.Create;
      with dummytest do
      begin
        FServer := StringGrid1.Cells[0, I];
        FPort := StrToIntDef(StringGrid1.Cells[1, I], 0);
        FEndpoint := StringGrid1.Cells[2, I];
        if StringGrid1.Cells[3, I].Contains('Basic') then
        begin
          FAuthType := atBasic;
          FAuthUser := StringGrid1.Cells[4, I];
          FAuthPassword := StringGrid1.Cells[5, I];
        end
        else
          FAuthType := atNone;

        if cbGET.IsChecked then
          FMethods := FMethods + [rtmGET];
        if cbPOST.IsChecked then
          FMethods := FMethods + [rtmPOST];
        if cbPUT.IsChecked then
          FMethods := FMethods + [rtmPUT];
        if cbPATCH.IsChecked then
          FMethods := FMethods + [rtmPATCH];
        if cbDELETE.IsChecked then
          FMethods := FMethods + [rtmDELETE];

        if rbSequencial.IsChecked then
          FTestType := ttSequencial
        else if rbParalelo.IsChecked then
        begin
          FTestType := ttConcorrente;
          FCcRequestCount := StrToIntDef(eConcorrentes.Text, 0);
        end;
        FRequestCount := StrToIntDef(eRequisicoes.Text, 0);
      end;
      SetLength(TestObjects, Length(TestObjects) + 1);
      TestObjects[High(TestObjects)] := dummytest;
    end;
  end;

end;

procedure TfPrincipal.EncerraTeste;
begin
  fim := now;
  FResultado.LogMessage('Testes finalizados após ' +
    FormatDateTime('hh:nn:ss:zzz', (fim - inicio)) + ' (hor:min:seg:mil)');
  FResultado.LogMessage(Format(' - Total: %d, Sucesso: %d, Falha: %d',
    [pass + fail, pass, fail]));
  FResultado.LogMessage('=======================================');
  LimparObjetosTeste;
end;

procedure TfPrincipal.FormCreate(Sender: TObject);
begin
  lVersao.Text := Format('Versão componentes: %s', [RALVERSION]);
  StringGrid1.RowCount := 0;
  FResultado := nil;
  Application.CreateForm(TFResultado, FResultado);
end;

procedure TfPrincipal.FormDestroy(Sender: TObject);
begin
  try
    FResultado.Free; // Porque? Porque o assigned não funciona
  except // basta ignorar o erro ou rodar em modo release
  end;
end;

procedure TfPrincipal.IniciarClick(Sender: TObject);
begin
  if not Assigned(FResultado) then
    Application.CreateForm(TFResultado, FResultado);
  FResultado.Show;
  inicio := now;

  FResultado.LogMessage('Testes sequenciais iniciados às ' + TimeToStr(inicio));
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
  FResultado.LogMessage('Testes sequenciais iniciados às ' + TimeToStr(inicio));
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
  I, J: integer;
  RESTClient: TRESTDAO;
begin
  DefinirParametrosTeste;
  if not Assigned(FResultado) then
    Application.CreateForm(TFResultado, FResultado);
  FResultado.Show;
  inicio := now;
  FResultado.LogMessage('Testes iniciados às %s', [TimeToStr(inicio)]);
  FResultado.LogMessage('------------------------------------------');

  for I := 0 to pred(Length(TestObjects)) do
  begin
    if cbRESTNativo.IsChecked then
    begin
      RESTClient := TRESTDAO.Create(TestObjects[I].FServer,
        TestObjects[I].FPort.ToString);

      if TestObjects[I].FAuthType = atBasic then
        RESTClient.SetBasicAuth(TestObjects[I].FAuthUser,
          TestObjects[I].FAuthPassword);

      FResultado.LogMessage('Testando servidor %s:%d com %d requisições',
        [TestObjects[I].FServer, TestObjects[I].FPort,
        TestObjects[I].FRequestCount]);
      if TestObjects[I].FTestType = ttSequencial then
      begin
        FResultado.LogMessage('Iniciando testes sequenciais com RESTClient...');

        if (rtmGET in TestObjects[I].FMethods) then
          TesteEndpointREST(TestObjects[I].FEndpoint, rtmGET,
            TestObjects[I].FRequestCount, RESTClient);

        if (rtmPOST in TestObjects[I].FMethods) then
          TesteEndpointREST(TestObjects[I].FEndpoint, rtmPOST,
            TestObjects[I].FRequestCount, RESTClient);

        if (rtmPUT in TestObjects[I].FMethods) then
          TesteEndpointREST(TestObjects[I].FEndpoint, rtmPUT,
            TestObjects[I].FRequestCount, RESTClient);

        if (rtmPATCH in TestObjects[I].FMethods) then
          TesteEndpointREST(TestObjects[I].FEndpoint, rtmPATCH,
            TestObjects[I].FRequestCount, RESTClient);

        if (rtmDELETE in TestObjects[I].FMethods) then
          TesteEndpointREST(TestObjects[I].FEndpoint, rtmDELETE,
            TestObjects[I].FRequestCount, RESTClient);

        FResultado.LogMessage('Fim dos testes sequenciais');
      end
      else
      begin
        FResultado.LogMessage
          ('Iniciando testes com concorrência de conexão com RESTClient...');
        for J := 0 to pred(TestObjects[I].FCcRequestCount) do
          TThread.CreateAnonymousThread(
            procedure
            var
              count: integer;
              ThreadedCli: TRESTDAO;
            begin
              ThreadedCli := TRESTDAO.Create(TestObjects[I].FServer,
                TestObjects[I].FPort.ToString);
              if TestObjects[I].FAuthType = atBasic then
                ThreadedCli.SetBasicAuth(TestObjects[I].FAuthUser,
                  TestObjects[I].FAuthPassword);

              count := TestObjects[I].FRequestCount div TestObjects[I]
                .FCcRequestCount;
              if (rtmGET in TestObjects[I].FMethods) then
                TesteEndpointREST(TestObjects[I].FEndpoint, rtmGET, count,
                  ThreadedCli);

              if (rtmPOST in TestObjects[I].FMethods) then
                TesteEndpointREST(TestObjects[I].FEndpoint, rtmPOST, count,
                  ThreadedCli);

              if (rtmPUT in TestObjects[I].FMethods) then
                TesteEndpointREST(TestObjects[I].FEndpoint, rtmPUT, count,
                  ThreadedCli);

              if (rtmPATCH in TestObjects[I].FMethods) then
                TesteEndpointREST(TestObjects[I].FEndpoint, rtmPATCH, count,
                  ThreadedCli);

              if (rtmDELETE in TestObjects[I].FMethods) then
                TesteEndpointREST(TestObjects[I].FEndpoint, rtmDELETE, count,
                  ThreadedCli);

              ThreadedCli.Free;
            end).Start;
      end;
      if Assigned(RESTClient) then
        RESTClient.Free;
    end;
  end;

end;

procedure TfPrincipal.TesteEndpointREST(aEndpoint: string;
metodo: TTestRequestMethod; count: integer; aClient: TRESTDAO);
var
  I: integer;
  ini, fim: Double;
  erro: string;
begin
  ini := now;
  FResultado.LogMessage('Testando %d requisições...', [count]);
  for I := 0 to count do
    if not aClient.TesteEndpoint(aEndpoint, metodo, erro) then
    begin
      FResultado.LogMessage('%s após %d requisições', [erro, I]);
      inc(fail);
      break;
    end;
  fim := now;
  inc(pass);

  FResultado.LogMessage(' - finalizado após %s (min:seg:mil)',
    [FormatDateTime('nn:ss:zzz', (fim - ini))]);
  FResultado.LogMessage('=======================================');
end;

procedure TfPrincipal.TesteRESTRequest(aClient: TRESTDAO);
var
  erro: string;
begin
  pass := 0;
  fail := 0;
  FResultado.LogMessage('Realizando testes de Requisição com REST nativos...');
  if (eServidor.Text = EmptyStr) or (ePorta.Text = EmptyStr) then
  begin
    FResultado.LogMessage('Erro: Configurações de servidor ou porta inválidas');
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
      FResultado.LogMessage('Método GET disponível');
      TesteEndpointREST(eEndpoint.Text, rtmGET, 100, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmGET, 1000, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmGET, 10000, aClient);

      FResultado.LogMessage('Teste GET concluído');
    end;

    if not aClient.TesteEndpoint(eEndpoint.Text, rtmPOST, erro) then
    begin
      FResultado.LogMessage(Format('Teste %s', [erro]));
      inc(fail);
    end
    else
    begin
      FResultado.LogMessage('Método POST disponível');
      TesteEndpointREST(eEndpoint.Text, rtmPOST, 100, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmPOST, 1000, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmPOST, 10000, aClient);

      FResultado.LogMessage('Teste POST concluído');
    end;

    if not aClient.TesteEndpoint(eEndpoint.Text, rtmPUT, erro) then
    begin
      FResultado.LogMessage(Format('Teste %s', [erro]));
      inc(fail);
    end
    else
    begin
      FResultado.LogMessage('Método PUT disponível');
      TesteEndpointREST(eEndpoint.Text, rtmPUT, 100, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmPUT, 1000, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmPUT, 10000, aClient);

      FResultado.LogMessage('Teste PUT concluído');
    end;

    if not aClient.TesteEndpoint(eEndpoint.Text, rtmPATCH, erro) then
    begin
      FResultado.LogMessage(Format('Teste %s', [erro]));
      inc(fail);
    end
    else
    begin
      FResultado.LogMessage('Método PATCH disponível');
      TesteEndpointREST(eEndpoint.Text, rtmPATCH, 100, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmPATCH, 1000, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmPATCH, 10000, aClient);

      FResultado.LogMessage('Teste PATCH concluído');
    end;

    if not aClient.TesteEndpoint(eEndpoint.Text, rtmDELETE, erro) then
    begin
      FResultado.LogMessage(Format('Teste %s', [erro]));
      inc(fail);
    end
    else
    begin
      FResultado.LogMessage('Método DELETE disponível');
      TesteEndpointREST(eEndpoint.Text, rtmDELETE, 100, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmDELETE, 1000, aClient);
      TesteEndpointREST(eEndpoint.Text, rtmDELETE, 10000, aClient);

      FResultado.LogMessage('Teste DELETE concluído');
    end;
  end;
  FResultado.LogMessage('Fim de testes de Requisição com REST nativos...');
  FResultado.LogMessage(Format('Testes realizados: %d, Sucesso: %d, Falhas: %d',
    [pass + fail, pass, fail]));
end;

end.
