/// Images shared by the screens (buttons of the IDE list, marks of the
/// features tree).
unit RALInst.Tela.Imagens;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type
  /// Holder of the shared image list.
  TImagens = class(TDataModule)
    imgList: TImageList;
  end;

var
  Imagens: TImagens;

implementation

{$R *.lfm}

end.
