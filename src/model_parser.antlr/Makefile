CLASS_FILES = BMCParser.class BMCLexer.class Main.class

$(CLASS_FILES): BMCParser.java BMCLexer.java Main.java
	CLASSPATH=/usr/share/java/antlr3-runtime.jar javac $+

BMCParser.java BMCLexer.java BMC.tokens: BMC.g
	antlr3 BMC.g

check: $(CLASS_FILES)
	CLASSPATH=/usr/share/java/antlr3-runtime.jar:. java Main < input_test.in > input_test.out
	diff input_test.out input_test_expected.out
	CLASSPATH=/usr/share/java/antlr3-runtime.jar:. java Main < var_test.in > var_test.out
	diff var_test.out var_test_expected.out
	CLASSPATH=/usr/share/java/antlr3-runtime.jar:. java Main < var_test_2.in > var_test_2.out
	diff var_test_2.out var_test_2_expected.out
	CLASSPATH=/usr/share/java/antlr3-runtime.jar:. java Main < var_test_halo.in > var_test_halo.out
	diff var_test_halo.out var_test_halo_expected.out
