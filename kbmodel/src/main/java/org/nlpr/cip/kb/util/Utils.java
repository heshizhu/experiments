package org.nlpr.cip.kb.util;

import java.io.PrintWriter;

public class Utils {
    public static final String ttlPrefix = "@prefix fb: <http://rdf.freebase.com/ns/>.";

    // Somewhat of a crude approximation.
    public static boolean isUrl(String s) {
        return s.startsWith("<");
    }

    // Virtuoso can't deal with this; these are probably useless anyway.
    public static boolean identifierContainsStrangeCharacters(String s) {
        return !s.startsWith("\"") && s.contains("$");
    }

    // Convert a string from the ns: namespace to the fb: namespace.
    // "ns:en.barack_obama" => "fb:en.barack_obama"
    public static String ns_to_fb(String s) {
        if (s.startsWith("ns:")) return "fb:" + s.substring(3);
        return s;
    }

    // "\"/en/distributive_writing\"" => "fb:en.distributive_writing"
    public static String string_to_rdf(String arg2) {
        if (!arg2.startsWith("\"/") || !arg2.endsWith("\""))
            throw new RuntimeException("Bad: " + arg2);
        return "fb:" + arg2.substring(2, arg2.length() - 1).replaceAll("/", ".");
    }

    public static String[] parseTriple(String line) {
        if (!line.endsWith(".")) return null;
        String[] tokens = line.substring(0, line.length() - 1).split("\t");
        if (tokens.length != 3) return null;
        tokens[0] = Utils.ns_to_fb(tokens[0]);
        tokens[1] = Utils.ns_to_fb(tokens[1]);
        tokens[2] = Utils.ns_to_fb(tokens[2]);
        return tokens;
    }

    public static int parseInt(String arg2) {
        if (!arg2.endsWith("^^xsd:int"))
            throw new RuntimeException("Arg2 is not a valid integer: " + arg2);
        int closingQuoteIndex = arg2.lastIndexOf('"');
        return Integer.parseInt(arg2.substring(1, closingQuoteIndex));
    }

    public static String parseStr(String arg2) {
        if (!arg2.endsWith("@en"))
            throw new RuntimeException("Arg2 is not a valid String: " + arg2);
        int closingQuoteIndex = arg2.lastIndexOf('"');
        return arg2.substring(1, closingQuoteIndex);
    }

    public static void writeTriple(PrintWriter out, String arg1, String property, String arg2) {
        out.println(arg1 + "\t" + property + "\t" + arg2 + ".");
    }

    // For some reason, the Freebase topic dumps don't have properly formatted numbers.
    // We need to replace
    //   fb:m.012_53     fb:people.person.height_meters  1.57.
    // with
    //   fb:m.012_53     fb:people.person.height_meters  "1.57"^^xsd:double.
    // This function operates on the second argument (value).
    public static String quoteValues(String value) {
        if (value.equals("true")) return "\"true\"^^xsd:boolean";
        if (value.equals("false")) return "\"false\"^^xsd:boolean";

        // Short circuit: not numeric
        if (value.startsWith("\"") || (value.length() > 0 && Character.isLetter(value.charAt(0))))
            return value;

        // Try to convert to integer
        try {
            Integer.parseInt(value);
            return "\"" + value + "\"^^xsd:int";
        } catch (NumberFormatException e) {
        }
        // Try to convert to double
        try {
            Double.parseDouble(value);
            return "\"" + value + "\"^^xsd:double";
        } catch (NumberFormatException e) {
        }
        return value;
    }



    public static boolean isURI(String term){
        return term.startsWith("<") && term.endsWith(">");
    }

    public static String convertURIToPrefix(String term) {
        if(isURI(term))
            return term.substring(1, term.length() - 1)
                    .replace("http://rdf.freebase.com/ns/", "fb:")
                    .replace("/", ".");
        return term;
    }

    public static String getQueryPrefix(){
        return String.format("prefix fb:<http://rdf.freebase.com/ns/>");
    }

}
