ANTLR_JAVA_OUTPUT = BMCParser.java BMCLexer.java 
#BMCTree.java
ANTLR_OUTPUT = $(ANTLR_JAVA_OUTPUT) BMC.tokens
JAVA_FILES = $(ANTLR_JAVA_OUTPUT) Main.java

CLASS_FILES = BMCParser.class BMCLexer.class Main.class

$(CLASS_FILES): $(JAVA_FILES)
	CLASSPATH=/usr/share/java/antlr3-runtime.jar:/usr/share/java/stringtemplate.jar javac $+

#BMCTree.g
$(ANTLR_OUTPUT): BMC.g 
	antlr3 $+

TESTS = input_test input_test2 input_test3 input_test4 input_test5 halo-model
check: $(CLASS_FILES)
	@for test in $(TESTS); do \
		echo "Running test: $${test}"; \
		CLASSPATH=/usr/share/java/antlr3-runtime.jar:/usr/share/java/stringtemplate.jar:. java Main $${test}.in > $${test}.out; \
		diff -u -w $${test}.out $${test}_expected.out; \
	done
