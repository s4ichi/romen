#!/Usr/bin/make -f

SRC= main.ml
COMPONENT= main.ml
TARGET= infer

all: $(TARGET)

$(TARGET): $(COMPONENT)
	ocamlfind ocamlmktop -annot -o $(TARGET) $(COMPONENT)

clean:
	/bin/rm -f $(TARGET)
	/bin/rm -f ./*.annot
	/bin/rm -f ./*.cmi
	/bin/rm -f ./*.cmo
