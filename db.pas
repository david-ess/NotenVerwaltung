unit db;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TNote }

  TNote = class
    private
      id: word;         // ID wird automatisch beim Erstellen der Note vergeben
      wert: byte;       // Wert der Note (0-15)
      ka: boolean;      // Kursarbeit?
      datum: TDateTime; // Datum und Uhrzeit der Eintragung
      fachId: byte;     // ID des Faches, zu welchem die Note gehört
    public
      function getWert: byte;
      procedure setWert(_wert: byte);
      function getKA: boolean;
      procedure setKA(_KA: boolean);
      function getDatum: TDateTime;
      procedure setDatum(_datum: TDateTime);
      function getFach: byte;
      procedure setFach(_fachId: byte);
      function getId: word;
  end;

  pTNote = ^TNote;
  pTNoteArray = Array of pTNote;

  { TFach }

  TFach = class
    private
      id: byte;       // ID des Faches (wird automatisch vergeben)
      name: String;   // Name des Faches
    public
      noten: pTNoteArray;
      function getName: String;
      procedure setName(_Name: String);
      procedure updateNotenListe;
  end;

  TNoteArray = Array of TNote;
  TFachArray = Array of TFach;

  { TDatenbank }

  TDatenbank = class
    private
      noten: TNoteArray;
    public
      faecher: TFachArray;
      function fachHinzu(_Name: String; _id: byte = 0): boolean;
      procedure fachLoeschen(_Name: String);
      procedure noteHinzu(_wert: byte; _ka: boolean; _datum: TDateTime; _fach: byte; _id: word = 0);
      procedure noteLoeschen(_id: word);         // word ist eine Ganzzahl im Bereich (0 .. 65535)
      procedure laden(pfad: String);
      procedure speichern(pfad: String);
      procedure init;                            // Initialisieren des Datenbankobjekts
      procedure frei;                            // Zerstören des Datenbankobjekts
      function getFachId(_name: String): byte;   // gibt die ID des angefragten Faches aus
  end;


Var Datenbank: TDatenbank;


implementation

{ TDatenbank }

function TDatenbank.fachHinzu(_Name: String; _id: byte = 0): boolean;
Var i, ind: integer;
    vorhanden: boolean;
begin
  result:=false;
  // Prüfen, ob es nicht schon ein Fach mit diesem Namen gibt
  vorhanden:=false;
  for i:=0 to length(faecher)-1 do
    begin
      if faecher[i].name = _Name then vorhanden:=true;
    end;
  // Fach (Objekt) erzeugen
  if not vorhanden then
    begin
      setLength(faecher, length(faecher)+1);
      ind := length(faecher)-1;             // index speichern, um ihn nicht immer wieder aufrufen zu müssen
      faecher[ind]:=TFach.Create;
      faecher[ind].name:=_Name;
      // ID ermitteln
      if _id=0 then
        begin
          if ind=0 then faecher[ind].id := 1
          else faecher[ind].id := faecher[ind-1].id+1;
        end
      else faecher[ind].id := _id;
      // Erfolg zurückmelden
      result:=true;
    end;
end;

procedure TDatenbank.fachLoeschen(_Name: String);
Var i, j: integer;
begin
  // Fach in Array suchen
  i:=0;
  while faecher[i].name <> _Name do i:=i+1;
  // Fach löschen
  for j:=i to length(faecher)-2 do
    begin
      faecher[j].name:=faecher[j+1].name;
      faecher[j].id:=faecher[j+1].id;
    end;
  // Objekt freigeben
  faecher[length(faecher)-1].Free;
  SetLength(faecher, length(faecher)-1);
end;

procedure TDatenbank.noteHinzu(_wert: byte; _ka: boolean; _datum: TDateTime; _fach: byte; _id: word = 0);
Var ind: integer;
begin
  // Note (Objekt) erzeugen
  SetLength(noten, length(noten)+1);
  ind := length(noten)-1;           // index speichern, um ihn nicht immer wieder aufrufen zu müssen
  noten[ind]:=TNote.Create;
  // Daten einlesen
  noten[ind].wert:=_wert;
  noten[ind].ka:=_ka;
  noten[ind].datum:=_datum;
  noten[ind].fachId:=_fach;
  // ID ermitteln
  if _id=0 then
    begin
      if ind=0 then noten[ind].id := 1
      else noten[ind].id := noten[ind-1].id+1;
    end
  else noten[ind].id:=_id;
end;

procedure TDatenbank.noteLoeschen(_id: word);
Var i, j: integer;
begin
  // Note in Array suchen
  i:=0;
  while noten[i].id <> _id do i:=i+1;
  // Note löschen
  for j:=i to length(noten)-2 do
    begin
      noten[j].wert:=noten[j+1].wert;
      noten[j].ka:=noten[j+1].ka;
      noten[j].datum:=noten[j+1].datum;
      noten[j].fachId:=noten[j+1].fachId;
      noten[j].id:=noten[j+1].id;
    end;
  // Objekt freigeben
  noten[length(noten)-1].Free;
  SetLength(noten, length(noten)-1);
end;

procedure TDatenbank.laden(pfad: String);
Var i, anzNoten, anzFaecher: Integer;
    stream: TFileStream;
    id: word;
    wert: byte;
    ka: boolean;
    datum: TDateTime;
    fachId: byte;
    name: String;
begin
  // Stream erzeugen
  stream := TFileStream.Create(pfad, fmOpenRead);
  // Header lesen
  anzFaecher:=stream.ReadWord;
  anzNoten:=stream.ReadWord;
  // Fächer hinzufügen
  For i:=0 to anzFaecher-1 do
    begin
      stream.ReadBuffer(id, SizeOf(byte));
      stream.ReadBuffer(name, SizeOf(String[20]));
      self.fachHinzu(name,id);
    end;
  // Noten hinzufügen
  For i:=0 to anzNoten-1 do
    begin
      stream.ReadBuffer(id, SizeOf(word));
      stream.ReadBuffer(wert, SizeOf(byte));
      stream.ReadBuffer(ka, SizeOf(boolean));
      stream.ReadBuffer(datum, SizeOf(TDateTime));
      stream.ReadBuffer(fachId, SizeOf(byte));
      self.noteHinzu(wert, ka, datum, fachId, id);
    end;
  // Stream freigeben
  stream.free;
end;

procedure TDatenbank.speichern(pfad: String);
Var i: Integer;
    stream: TFileStream;
begin
  // Stream erzeugen
  if fileExists(pfad)
  then stream := TFileStream.Create(pfad, fmOpenReadWrite)
  else stream := TFileStream.Create(pfad, fmCreate);
  // Header schreiben
  stream.WriteWord(length(faecher));
  stream.WriteWord(length(noten));
  // Fächer schreiben
  For i:=0 to length(faecher)-1 do
    begin
      stream.WriteBuffer(faecher[i].id, SizeOf(byte));
      stream.WriteBuffer(faecher[i].name, SizeOf(String[20]));
    end;
  // Noten schreiben
  For i:=0 to length(noten)-1 do
    begin
      stream.WriteBuffer(noten[i].id, SizeOf(word));
      stream.WriteBuffer(noten[i].wert, SizeOf(byte));
      stream.WriteBuffer(noten[i].ka, SizeOf(boolean));
      stream.WriteBuffer(noten[i].datum, SizeOf(TDateTime));
      stream.WriteBuffer(noten[i].fachId, SizeOf(byte));
    end;
  // Stream freigeben
  stream.free;
end;

procedure TDatenbank.init;
begin
   // Arrays initialisieren, indem die Längen auf Null gesetzt werden
   setLength(faecher, 0);
   setLength(noten, 0);
end;

procedure TDatenbank.frei;
Var i: integer;
begin
  // Fächer aus RAM löschen
  for i:=0 to length(faecher)-1 do
    begin
      faecher[i].Free;
    end;
  SetLength(faecher, 0);
  // Noten aus RAM löschen
  for i:=0 to length(noten)-1 do
    begin
      noten[i].Free;
    end;
  SetLength(noten, 0);
end;

function TDatenbank.getFachId(_name: String): byte;
Var i: integer;
begin
  i:=0;
  while faecher[i].name <> _name do i:=i+1;
  result:=faecher[i].id;
end;

{ TFach }

function TFach.getName: String;
begin
  result:=self.name;
end;

procedure TFach.setName(_Name: String);
begin
  self.name:=_Name;
end;

procedure TFach.updateNotenListe;
Var i: Integer;
begin
  // Alle Noten durchgehen und Zeiger auf die speichern, welche zum Fach gehören
  for i:=0 to length(Datenbank.noten)-1 do
    begin
      SetLength(noten, 0);
      if Datenbank.noten[i].fachId = self.id then
        begin
          SetLength(noten, length(noten)+1);
          noten[length(noten)-1] := @Datenbank.noten[i];
        end;
    end;
end;

{ TNote }

function TNote.getWert: byte;
begin
  result:=self.wert;
end;

procedure TNote.setWert(_wert: byte);
begin
  self.wert:=_wert;
end;

function TNote.getKA: boolean;
begin
  result:=self.ka;
end;

procedure TNote.setKA(_KA: boolean);
begin
  self.ka:=_KA;
end;

function TNote.getDatum: TDateTime;
begin
  result:=self.datum;
end;

procedure TNote.setDatum(_datum: TDateTime);
begin
  self.datum:=_datum;
end;

function TNote.getFach: byte;
begin
  result:=self.fachId;
end;

procedure TNote.setFach(_fachId: byte);
begin
  self.fachId:=_fachId;
end;

function TNote.getId: word;
begin
  result:=self.id;
end;

end.

