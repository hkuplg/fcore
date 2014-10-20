all : fractals.sf
	cd lib; javac *.java
	f2j -r -m stack $^

.PHONY : clean
clean :
	rm -f lib/*.class *.class *.java
