# Generic Array Helper

## Beschreibung

Diese Unit definiert eine generische Hilfsklasse zur Erstellung dynamischer Arrays für verschiedene Datentypen. Die Methode `Create` erlaubt die Initialisierung eines Arrays mit einer angegebenen Länge und gibt das Array zurück.

## Kompatibilität

- **Free Pascal** (Delphi-Modus)
- **Delphi**

## Verwendung

Die generische Klasse `TArrayHelper<T>` kann zur Erzeugung dynamischer Arrays für verschiedene Datentypen verwendet werden.

### Beispiel

```pascal
// Erzeuge ein Integer-Array mit 5 Elementen
var IntArr: TArray<Integer>;
IntArr := TArrayHelper<Integer>.Create(5);
WriteLn('Integer-Array Länge: ', Length(IntArr)); // Erwartet: 5

// Erzeuge ein Boolean-Array mit 3 Elementen
var BoolArr: TArray<Boolean>;
BoolArr := TArrayHelper<Boolean>.Create(3);
WriteLn('Boolean-Array Länge: ', Length(BoolArr)); // Erwartet: 3
```

## Installation

1. Kopiere die Datei in dein Projektverzeichnis.
2. Binde die Unit in dein Projekt ein:
   ```pascal
   uses YourUnitName;
   ```

## Lizenz

Dieses Projekt steht unter der **MIT-Lizenz**. Du kannst den Code frei nutzen, modifizieren und weitergeben.

## Autor

Erstellt von paule32

