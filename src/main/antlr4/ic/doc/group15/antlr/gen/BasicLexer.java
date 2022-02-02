// Generated from C:/Users/ameen/Documents/ameen/aaimperial/COMP50007/wacc1/WACC_15/src/main/antlr4/ic/doc/group15/antlr\BasicLexer.g4 by ANTLR 4.9.2
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class BasicLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.9.2", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		PLUS=1, MINUS=2, OPEN_PARENTHESES=3, CLOSE_PARENTHESES=4, INTEGER=5, SKIP_STAT=6, 
		BEGIN=7, END=8, WS=9, COMMENT=10;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"PLUS", "MINUS", "OPEN_PARENTHESES", "CLOSE_PARENTHESES", "DIGIT", "INTEGER", 
			"SKIP_STAT", "BEGIN", "END", "WS", "COMMENT"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'+'", "'-'", "'('", "')'", null, "'skip'", "'begin'", "'end'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "PLUS", "MINUS", "OPEN_PARENTHESES", "CLOSE_PARENTHESES", "INTEGER", 
			"SKIP_STAT", "BEGIN", "END", "WS", "COMMENT"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}


	public BasicLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "BasicLexer.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\fI\b\1\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\3\2\3\2\3\3\3\3\3\4\3\4\3\5\3\5\3\6\3\6\3\7\6\7%\n\7\r\7\16"+
		"\7&\3\b\3\b\3\b\3\b\3\b\3\t\3\t\3\t\3\t\3\t\3\t\3\n\3\n\3\n\3\n\3\13\6"+
		"\139\n\13\r\13\16\13:\3\13\3\13\3\f\3\f\7\fA\n\f\f\f\16\fD\13\f\3\f\3"+
		"\f\3\f\3\f\2\2\r\3\3\5\4\7\5\t\6\13\2\r\7\17\b\21\t\23\n\25\13\27\f\3"+
		"\2\4\5\2\13\f\17\17\"\"\3\2\f\f\2J\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2"+
		"\2\t\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3"+
		"\2\2\2\2\27\3\2\2\2\3\31\3\2\2\2\5\33\3\2\2\2\7\35\3\2\2\2\t\37\3\2\2"+
		"\2\13!\3\2\2\2\r$\3\2\2\2\17(\3\2\2\2\21-\3\2\2\2\23\63\3\2\2\2\258\3"+
		"\2\2\2\27>\3\2\2\2\31\32\7-\2\2\32\4\3\2\2\2\33\34\7/\2\2\34\6\3\2\2\2"+
		"\35\36\7*\2\2\36\b\3\2\2\2\37 \7+\2\2 \n\3\2\2\2!\"\4\62;\2\"\f\3\2\2"+
		"\2#%\5\13\6\2$#\3\2\2\2%&\3\2\2\2&$\3\2\2\2&\'\3\2\2\2\'\16\3\2\2\2()"+
		"\7u\2\2)*\7m\2\2*+\7k\2\2+,\7r\2\2,\20\3\2\2\2-.\7d\2\2./\7g\2\2/\60\7"+
		"i\2\2\60\61\7k\2\2\61\62\7p\2\2\62\22\3\2\2\2\63\64\7g\2\2\64\65\7p\2"+
		"\2\65\66\7f\2\2\66\24\3\2\2\2\679\t\2\2\28\67\3\2\2\29:\3\2\2\2:8\3\2"+
		"\2\2:;\3\2\2\2;<\3\2\2\2<=\b\13\2\2=\26\3\2\2\2>B\7%\2\2?A\n\3\2\2@?\3"+
		"\2\2\2AD\3\2\2\2B@\3\2\2\2BC\3\2\2\2CE\3\2\2\2DB\3\2\2\2EF\7\f\2\2FG\3"+
		"\2\2\2GH\b\f\2\2H\30\3\2\2\2\6\2&:B\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}