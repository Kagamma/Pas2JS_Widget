unit Unit1;

{$mode ObjFPC}{$H+}

interface

uses
  JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls;

type

  { TWForm2 }

  TWForm2 = class(TWForm)
    WButton1: TWButton;
    procedure WButton1Click(Sender: TObject);
  private

  public

  end;

var
  WForm2: TWForm2;

implementation

uses
  main;

{$R *.lfm}

{ TWForm2 }

procedure TWForm2.WButton1Click(Sender: TObject);
begin
  WForm1.Show;
end;

end.
