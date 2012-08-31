/*
import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;

public class Main {
	public static void main(String[] args) throws Exception {
		CharStream input = new ANTLRFileStream(args[0]);
		BMCLexer lex = new BMCLexer(input);
		CommonTokenStream tokens = new CommonTokenStream(lex);
		BMCParser parser = new BMCParser(tokens);
		BMCParser.input_return r = parser.input();

		// DEBUG
		//System.err.println("tree: "+((Tree)r.tree).toStringTree());

		//CommonTree r0 = ((CommonTree)r.tree);
		//CommonTreeNodeStream nodes = new CommonTreeNodeStream(r0);
		//nodes.setTokenStream(tokens);
		//BMCTree walker = new BMCTree(nodes);
		//walker.input();
	}
}
*/

import java.io.*;
import org.antlr.runtime.*;
import org.antlr.stringtemplate.*;
import org.antlr.stringtemplate.language.*;

public class Main {
    public static StringTemplateGroup templates;

    public static void main(String[] args) throws Exception {
//	String templateFileName;
	int a = 0;
//	if ( args.length<=1 ) {
//		templateFileName = "Java.stg";
//	}
//	else {
//		templateFileName = args[a];
//		a++;
//	}
	//templates = new StringTemplateGroup(new FileReader(templateFileName),
//					    AngleBracketTemplateLexer.class);

	CharStream input = new ANTLRFileStream(args[a]);
	BMCLexer lexer = new BMCLexer(input);
	CommonTokenStream tokens = new CommonTokenStream(lexer);
	BMCParser parser = new BMCParser(tokens);
	//parser.setTemplateLib(templates);
	RuleReturnScope r = parser.input();
	System.out.println(r.getTemplate().toString());
    }
}
