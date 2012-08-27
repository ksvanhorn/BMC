import org.antlr.runtime.*;
public class Main {
	public static void main(String[] args) throws Exception {
		// create a CharStream that reads from standard input
		ANTLRInputStream input = new ANTLRInputStream(System.in);
		// create a lexer that feeds off of input CharStream
		BMCLexer lexer = new BMCLexer(input);
		// create a buffer of tokens pulled from the lexer
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		// create a parser that feeds off the tokens buffer
		BMCParser parser = new BMCParser(tokens);
		// begin parsing at rule input
		parser.input();
	}
}
