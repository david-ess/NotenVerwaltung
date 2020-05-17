unit HF_Notenverwaltung;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, db;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    ListBox1: TListBox;
    SpinEdit1: TSpinEdit;
    procedure Button2Click(Sender: TObject);
    procedure ComboBox1Enter(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  datenbankPfad: String;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button2Click(Sender: TObject);
begin
  ShowMessage('Hallo Welt!');
end;

procedure TForm1.ComboBox1Enter(Sender: TObject);
Var i: integer;
begin
  ComboBox1.Items.Clear;
  for i:=0 to length(Datenbank.faecher) do
    begin
      ComboBox1.Items.Add(Datenbank.faecher[i].getName);
    end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Datenbank.speichern(datenbankPfad);
  Datenbank.frei;
  Datenbank.Destroy;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  datenbankPfad := ExtractFilePath(ParamStr(0)) + 'datenbank.dat';
  Datenbank:=TDatenbank.Create;
  Datenbank.init;
  Datenbank.laden(datenbankPfad);
end;

end.

