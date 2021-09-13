namespace RemObjects.Elements.RTL.Delphi;

{$IF NOT ISLAND AND NOT TOFFEE}

//84838: Toffee: IE
//84837: Island: cannot find operator to evaluate "nullable Char" = "nullable Char"

interface

uses
  RemObjects.Elements.RTL,
  RemObjects.Elements.RTL.Delphi;

type
  EFormatException = class({$IF ECHOES}System.FormatException{$ELSE}Exception{$ENDIF});

function Format(const AStr: String; Args: array of const): String;

implementation

function ParseInt(const AStr: String): Integer;
begin
  result := RemObjects.Elements.RTL.Convert.TryToInt32(AStr);
end;

function Format(const AStr: String; Args: array of const): String;
begin
  var ConvertedFormat := new TStringBuilder();
  const digits = ['0','1','2','3','4','5','6','7','8','9'];
  const typeIdentifiers = ['d', 'u', 'e', 'f', 'g', 'n', 'm', 'p', 's', 'x'];
  const validCharacters = digits + typeIdentifiers + [':', '-', '.'];

  var StrIndex := low(AStr);
  while StrIndex <= high(AStr) do
  begin
    var currentCharacter := AStr[StrIndex];
    if (currentCharacter = '%') and (StrIndex <= high(AStr)) then
    begin
      var nextCharacter := AStr[StrIndex + 1];
      if nextCharacter = '%' then
      begin
        ConvertedFormat.Append(currentCharacter);
        inc(StrIndex);
      end
      else if nextCharacter in validCharacters then
      begin
        var &index: Integer := 0;
        var alignmentChar: nullable Char := nil;
        var width: nullable Integer := nil;
        var precision: nullable Integer := nil;
        var &type: nullable Char := nil;

        currentCharacter := nextCharacter;
        inc(StrIndex);
        var currentNumberString := '';
        while (currentCharacter in validCharacters) and (StrIndex <= high(AStr)) do
        begin
          currentCharacter := AStr[StrIndex];

          if currentCharacter in digits then
          begin
            currentNumberString := currentNumberString + currentCharacter;
          end
          else if currentCharacter = ':' then
          begin
            &index := ParseInt(currentNumberString);
            currentNumberString := '';
          end
          else if currentCharacter = '-' then
          begin
            if alignmentChar <> nil then
              raise new EFormatException("Left justification can only be specified once");

            alignmentChar := currentCharacter;
          end
          else if currentCharacter = '.' then
          begin
            if not String.IsNullOrEmpty(currentNumberString) then
            begin
              width := ParseInt(currentNumberString);
              currentNumberString := '';
            end;
          end
          else if currentCharacter in typeIdentifiers then
          begin
            if not String.IsNullOrEmpty(currentNumberString) then
            begin
              var intValue := ParseInt(currentNumberString);

              if width = nil then
                width := intValue
              else
                precision := intValue;

              currentNumberString := '';
            end;

            &type := currentCharacter;
            break;
          end;

          inc(StrIndex);
        end;

        if &type = nil then
          raise new EFormatException("Type character not found in format marker");

        case &type of
          'm':
            &type := 'C';
          'p':
            &type := 'X';
        end;

        ConvertedFormat.Append('{');
        ConvertedFormat.Append(&index);
        if width <> nil then
        begin
          ConvertedFormat.Append(',');
          if alignmentChar <> nil then
            ConvertedFormat.Append(alignmentChar);
          ConvertedFormat.Append(width);
        end;
        ConvertedFormat.Append(':');
        ConvertedFormat.Append(&type);
        if precision <> nil then
          ConvertedFormat.Append(precision);
        ConvertedFormat.Append('}');

        inc(&index);
      end
      else
        raise new EFormatException("Unsupported character after the % marker: " + nextCharacter);
    end
    else
    begin
      ConvertedFormat.Append(currentCharacter);
      if currentCharacter = '{' then
        ConvertedFormat.Append('{');
      if currentCharacter = '}' then
        ConvertedFormat.Append('}');
    end;

    inc(StrIndex);
  end;

  result := RemObjects.Elements.RTL.String.Format(ConvertedFormat.ToString, Args);
end;

{$ENDIF}

end.