program demo; 

{$mode delphi}{$H+}

uses
  Interfaces, Forms, main, Unit1, Unit2;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TWForm1, WForm1);
  Application.CreateForm(TWForm2, WForm2);
  Application.CreateForm(TWForm3, WForm3);
  Application.Run;
end.