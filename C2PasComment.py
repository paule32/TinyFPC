# run this script before compile FPC files
import os
import re

def convert_c_to_pascal_comments_in_file(filepath):
    with open(filepath, "r", encoding="utf-8") as f:
        content = f.read()

    # Ersetze /** durch (**
    content = re.sub(r"/\*\*", "(**!", content)
    # Ersetze */ durch *)
    content = re.sub(r"\*/", "*)", content)

    with open(filepath, "w", encoding="utf-8") as f:
        f.write(content)
    print("Konvertiert: " + filepath)

def convert_directory(directory):
    for root, _, files in os.walk(directory):
        for file in files:
            if file.endswith((".pas", ".pp", ".p")):
                convert_c_to_pascal_comments_in_file(os.path.join(root, file))

if __name__ == "__main__":
    import sys
    if len(sys.argv) != 2:
        print("Verwendung: python c2pascal.py <datei oder ordner>")
    else:
        path = sys.argv[1]
        if os.path.isfile(path):
            convert_c_to_pascal_comments_in_file(path)
        elif os.path.isdir(path):
            convert_directory(path)
        else:
            print("Pfad nicht gefunden:", path)
