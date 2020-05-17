unit HF_Notenverwaltung;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Dialogs, StdCtrls, Spin, db;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    ListBox1: TListBox;
    SpinEdit1: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ComboBox1Enter(Sender: TObject);
    procedure ComboBox1KeyPress(Sender: TObject; var Key: char);
    procedure ComboBox1Select(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  datenbankPfad: String;
  Datenbank: TDatenbank;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button2Click(Sender: TObject); // Note löschen
begin
  if ListBox1.ItemIndex >= 0 then
    begin
      Datenbank.noteLoeschen(Datenbank.fachNoten[ListBox1.ItemIndex]^.getId);
      ListBox1.Items.Delete(ListBox1.ItemIndex);
    end
  else ShowMessage('Bitte die zu löschende Note markieren!');
end;

procedure TForm1.Button1Click(Sender: TObject); // Note hinzufügen oder ändern
begin
  if ListBox1.ItemIndex = -1 then // Note hinzufügen
    begin
      Datenbank.noteHinzu(SpinEdit1.Value, CheckBox1.Checked, StrToDate(Edit1.Text), Datenbank.getFachId(ComboBox1.Text));
    end
  else  // Note ändern
    begin
      Datenbank.fachNoten[ListBox1.ItemIndex]^.setWert(SpinEdit1.Value);
      Datenbank.fachNoten[ListBox1.ItemIndex]^.setKA(CheckBox1.Checked);
      Datenbank.fachNoten[ListBox1.ItemIndex]^.setDatum(StrToDate(Edit1.Text));
      Datenbank.fachNoten[ListBox1.ItemIndex]^.setFach(Datenbank.getFachId(ComboBox1.Text));
    end;
  ComboBox1Select(self);
end;

procedure TForm1.Button4Click(Sender: TObject);  // Fach löschen
begin
  if ComboBox1.Text <> 'Fach wählen' then
    begin
      Datenbank.fachLoeschen(ComboBox1.Text);
      ComboBox1Enter(self);
    end;
end;

procedure TForm1.ComboBox1Enter(Sender: TObject); // Fächer in ComboBox schreiben
Var i: integer;
begin
  ComboBox1.Items.Clear;
  for i:=0 to length(Datenbank.faecher)-1 do
    begin
      ComboBox1.Items.Add(Datenbank.faecher[i].getName);
    end;
end;

procedure TForm1.ComboBox1KeyPress(Sender: TObject; var Key: char);
begin
  // Wenn Enter gedrückt wird, neues Fach anlegen oder das Fach wählen, falls es schon existiert
  if Key = #13 then
      begin
        if not Datenbank.fachHinzu(ComboBox1.Text) then ComboBox1Select(self)
        else ComboBox1Enter(self); // ComboBox update
      end;
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
Var i: integer;
    eintrag: String;
begin
  // Zeiger auf Noten in das FachNotenArray laden
  Datenbank.ladeFachNoten(Datenbank.getFachId(ComboBox1.Text));
  // Noten in ListBox schreiben
  ListBox1.Items.Clear;
  for i:=0 to length(Datenbank.fachNoten)-1 do
    begin
      eintrag:=IntToStr(Datenbank.fachNoten[i]^.getWert);
      if Datenbank.fachNoten[i]^.getKA then eintrag := eintrag + ' (KA)';
      ListBox1.Items.Add(eintrag);
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
  Edit1.Text:=DateToStr(now);
end;

procedure TForm1.ListBox1SelectionChange(Sender: TObject; User: boolean);  // Felder mit Daten füllen
begin
  SpinEdit1.Value := Datenbank.fachNoten[ListBox1.ItemIndex]^.getWert;
  Edit1.Text := DateToStr(Datenbank.fachNoten[ListBox1.ItemIndex]^.getDatum);
  CheckBox1.Checked := Datenbank.fachNoten[ListBox1.ItemIndex]^.getKA;
end;

end.

