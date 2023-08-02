unit uPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.DateUtils, System.Rtti,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.Edit,
  FMX.Layouts, FMX.Objects, FMX.ListBox, FMX.TabControl, FMX.Grid.Style,
  FMX.Grid,

  RALConsts,

  DAO.Base, DAO.REST, DAO.RALIndy, DAO.RALNetHttp,

  TestUnit

    ;

type
  TRESTClientKind = (rckREST, rckRALIndy, rckRALSynopse, rckRALNetHttp);

  TfPrincipal = class(TForm)
    Layout1: TLayout;
    Image1: TImage;
    lVersao: TLabel;
    TabControl1: TTabControl;
    tiStress: TTabItem;
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
    cbRALIndy: TCheckBox;
    cbRALSynopse: TCheckBox;
    tiServer: TTabItem;
    Layout5: TLayout;
    FlowLayout1: TFlowLayout;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    FlowLayout2: TFlowLayout;
    Edit3: TEdit;
    Label4: TLabel;
    Edit4: TEdit;
    Label5: TLabel;
    Edit5: TEdit;
    Label6: TLabel;
    FlowLayout3: TFlowLayout;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox16: TCheckBox;
    CheckBox17: TCheckBox;
    CheckBox18: TCheckBox;
    CheckBox19: TCheckBox;
    CheckBox20: TCheckBox;
    CheckBox21: TCheckBox;
    CheckBox22: TCheckBox;
    CheckBox23: TCheckBox;
    FlowLayout7: TFlowLayout;
    Button1: TButton;
    cbRALNetHttp: TCheckBox;
    tiLog: TTabItem;
    Memo1: TMemo;
    FlowLayout8: TFlowLayout;
    Button2: TButton;
    tiCliente: TTabItem;
    procedure FormCreate(Sender: TObject);
    procedure Rectangle1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Rectangle2Click(Sender: TObject);
    procedure bAdicionarClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure bRemoverClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    TestObjects: array of TTestObject;
    procedure DefinirParametrosTeste(aClient: TRESTClientKind; aMemo: TMemo);
    procedure LimparObjetosTeste;
    procedure Testar;
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
var
  path: string;
begin
  path := ExtractFileDir(ParamStr(0)) + '\logTestTool.txt';
  Memo1.Lines.SaveToFile(path);
  Memo1.Lines.Add('Log salvo no arquivo: ' + path);
end;

procedure TfPrincipal.Button5Click(Sender: TObject);
begin
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
        RESTClient := TRALIndyDAO.Create(StringGrid1.Cells[0, I],
          StringGrid1.Cells[1, I]);

      rckRALNetHttp:
        RESTClient := TRALNetHttpDAO.Create(StringGrid1.Cells[0, I],
          StringGrid1.Cells[1, I]);
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

procedure TfPrincipal.FormCreate(Sender: TObject);
begin
  lVersao.Text := Format('Versão componentes: %s', [RALVERSION]);
  StringGrid1.RowCount := 0;
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
begin
  LimparObjetosTeste;

  if StringGrid1.RowCount > 0 then
  begin
    try
      if cbRESTNativo.IsChecked then
        DefinirParametrosTeste(rckREST, Memo1);

      if cbRALIndy.IsChecked then
        DefinirParametrosTeste(rckRALIndy, Memo1);

      if cbRALSynopse.IsChecked then
        DefinirParametrosTeste(rckRALSynopse, Memo1);

      if cbRALNetHttp.IsChecked then
        DefinirParametrosTeste(rckRALNetHttp, Memo1);

      TabControl1.ActiveTab := tiLog;
      for I := 0 to pred(Length(TestObjects)) do
        TestObjects[I].Test;
    finally

    end;
  end;
end;

end.
