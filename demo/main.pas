unit main;

{$mode objfpc}{$H+}

interface

uses
  JS, web, Classes, SysUtils, Graphics, Controls, Forms, WebCtrls, Menus, Unit1,
  Unit2, Dialogs;

type

  { TWForm1 }

  TWForm1 = class(TWForm)
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    WButton1: TWButton;
    WButton2: TWButton;
    WButton3: TWButton;
    WButton4: TWButton;
    lblUserName: TWLabel;
    WButton5: TWButton;
    WLabel1: TWLabel;
    WLabel2: TWLabel;
    WPanel1: TWPanel;
    WPanel10: TWPanel;
    WPanel11: TWPanel;
    WPanel2: TWPanel;
    WPanel3: TWPanel;
    WPanel4: TWPanel;
    WPanel5: TWPanel;
    WPanel6: TWPanel;
    WPanel7: TWPanel;
    WPanel8: TWPanel;
    WPanel9: TWPanel;
    WPopupMenu1: TWPopupMenu;
    WSplitter1: TWSplitter;
    WSplitter2: TWSplitter;
    WTimer1: TWTimer;
    procedure FormCreate(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure WButton1Click(Sender: TObject);
    procedure WButton2Click(Sender: TObject);
    procedure WButton3Click(Sender: TObject);
    procedure WButton5Click(Sender: TObject);
    procedure WPanel5Click(Sender: TObject);
    procedure WPanel8Click(Sender: TObject);
    procedure WPanel9Click(Sender: TObject);
    procedure WPanel9MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: nativeint);
    procedure WPanel9MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: nativeint);
    procedure WPanel9MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: nativeint);
    procedure WTimer1Timer(Sender: TObject);
  private
    FDrag: boolean;
    function test(aEvent: TJSMouseEvent): boolean;

  public

  end;

var
  WForm1: TWForm1;

implementation

{$R *.lfm}

{ TWForm1 }


procedure TWForm1.WButton5Click(Sender: TObject);
begin
  lblUserName.Caption := 'DEMO';
end;

procedure TWForm1.WPanel5Click(Sender: TObject);
begin
  writeln(WPanel5.Left, ' = ', WPanel5.Top);
end;

procedure TWForm1.WPanel8Click(Sender: TObject);
begin
  writeln('Click2');
  //WPanel8.Canvas.Rectangle(10, 10, 50, 50);
end;

procedure TWForm1.WPanel9Click(Sender: TObject);
begin
  writeln('Click3');
  //WPanel9.Canvas.Rectangle(10, 10, 50, 50);
end;

procedure TWForm1.WPanel9MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: nativeint);
begin
  writeln('Down');
  FDrag := True;
end;

procedure TWForm1.WPanel9MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: nativeint);
begin
  if FDrag then
  begin
    writeln(x, ' - ', y);
    //    WPanel9.Left := x;
    //    WPanel9.Top := y;
  end;
  writeln('Move');
end;

procedure TWForm1.WPanel9MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: nativeint);
begin
  writeln('Up');
  FDrag := False;
end;

procedure TWForm1.WTimer1Timer(Sender: TObject);
begin
  WLabel1.Caption := DateTimeToStr(Now);
end;

function TWForm1.test(aEvent: TJSMouseEvent): boolean;
begin
  writeln('Global');
end;

procedure TWForm1.WButton1Click(Sender: TObject);
begin
  if WPanel5.Color = clGreen then
    WPanel5.Color := clRed
  else
    WPanel5.Color := clGreen;
end;

procedure TWForm1.WButton2Click(Sender: TObject);
begin
  writeln(Application.ActiveForm.Caption);
  WForm2.Show;
  writeln(Application.ActiveForm.Caption);
end;

procedure TWForm1.WButton3Click(Sender: TObject);
begin
  if WPanel9.Alpha = 100 then
    WPanel9.Alpha := 255
  else
    WPanel9.Alpha := 100;
end;

procedure TWForm1.FormCreate(Sender: TObject);
begin
  document.onclick := @test;
end;

procedure TWForm1.MenuItem2Click(Sender: TObject);
begin
  ShowMessage('It''s a Show Message Dialog');
end;

end.
