package org.nlpr.cip.kb.corpus;

import com.google.common.collect.Sets;
import org.apache.log4j.Logger;
import org.nlpr.cip.kb.util.SparqlEndpoint;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;

/**
 * 得到所有满足规则的实例化命题
 * User: hesz
 * Date: 2015-3-5
 * Time: 15:38
 */
public class RuleGrounded {
    private static Logger log = Logger.getLogger(RuleGrounded.class);
    private static String basePath = "G:\\temp\\TransX\\";

    private String corpus;
    private String atomsPath;
    private SparqlEndpoint endpoint;

    public RuleGrounded(String corpus){
        this.corpus = corpus;
        this.atomsPath = String.format("%s%s/data/formulas/", basePath, corpus);
        Path path = FileSystems.getDefault().getPath(atomsPath);
        if(Files.notExists(path)) {
            try {
                Files.createDirectory(path);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        endpoint = SparqlEndpoint.getSparqlEndpoint("172.18.28.117", 5001);
    }

    public void grounding() throws IOException {
        Path input = FileSystems.getDefault().getPath(basePath, corpus, "data/rules.txt");
        Path output = FileSystems.getDefault().getPath(basePath, corpus, "data/formulas/formulas.txt");
        BufferedReader reader = Files.newBufferedReader(input, Charset.forName("utf-8"));
        BufferedWriter writer = Files.newBufferedWriter(output, Charset.forName("utf-8"));

        int id = 0;
        String line;
        while(true){
            line = reader.readLine();
            if(line == null) break;
            if(line.startsWith("#")) continue;
            if(line.trim().length() == 0) continue;
            String newline = groundingFormula(line, id++);
            writer.write(newline);
            writer.newLine();
        }

        writer.close();
        reader.close();
    }



    public String groundingFormula(String formula, int id) throws IOException {
        Path output = FileSystems.getDefault().getPath(atomsPath, String.format("%d.atoms", id));
        BufferedWriter writer = Files.newBufferedWriter(output, Charset.forName("utf-8"));

        String[] terms = formula.split(" ");
        if(terms.length % 3 != 0){
            System.out.println("input error: not a valid rule");
            System.exit(0);
        }
        Set<String> free_variables = Sets.newHashSet();
        for(int k = 0; k < terms.length; k ++){
            if(terms[k].startsWith("x_")){
                free_variables.add(terms[k]);
                terms[k] = "?" + terms[k];
            }
            else
                terms[k] = "<" + terms[k] + ">";
        }
        StringBuilder sparql = new StringBuilder();

        //首先获得共现频率
        sparql.append("SELECT COUNT( * )WHERE {\n");
        for(int i = 0; i < terms.length; i += 3){
            String triple = String.format("%s\t%s\t%s .\n", terms[i], terms[i+1], terms[i+2]);
            sparql.append(triple);
        }
        sparql.append("}");
//        System.out.println(sparql.toString());
        long supportFrq = endpoint.count(sparql.toString());


        //互动区其支持度
        sparql = new StringBuilder();
        sparql.append("SELECT COUNT( * )WHERE {\n");
        for(int i = 0; i < terms.length - 3; i += 3){
            String triple = String.format("%s\t%s\t%s .\n", terms[i], terms[i+1], terms[i+2]);
            sparql.append(triple);
        }
        sparql.append("}");
//        System.out.println(sparql.toString());
        long bodySupportFrq = endpoint.count(sparql.toString());
        double support = supportFrq * 1.0 / bodySupportFrq;


        int variable_num = free_variables.size();
        sparql = new StringBuilder();
        sparql.append("SELECT ");
        for(int i = 1; i <= variable_num; i  ++)
            sparql.append(" ?x_" + i + " ");
        sparql.append( " WHERE {\n");
        for(int i = 0; i < terms.length; i += 3){
            String triple = String.format("%s\t%s\t%s .\n", terms[i], terms[i+1], terms[i+2]);
            sparql.append(triple);
        }
        sparql.append("}");
//        System.out.println(sparql.toString());
        List<String> results = endpoint.query(sparql.toString());
        for(int i = 1; i < results.size(); i ++){
            String[] elements = results.get(i).split("\t");
            for(int k = 0; k < elements.length; k++){
                if(elements[k].startsWith("\"") && elements[k].endsWith("\""))
                    elements[k] = elements[k].substring(1, elements[k].length() - 1);
            }
            String groundFormula = formula;
            for(int k = 0; k < variable_num; k++)
                groundFormula = groundFormula.replaceAll(("x_" + (k+1)), elements[k]);
            writer.write(groundFormula);
            writer.newLine();
        }

        writer.close();
        return String.format("%s\t%d\t%d\t%f", formula, bodySupportFrq, supportFrq, support);
    }

    public static void main(String[] args) throws IOException {

        String corpus = "fb13";
        RuleGrounded ruleGrounded = new RuleGrounded(corpus);
        ruleGrounded.grounding();


//        String formula = "x_1 children x_2 x_2 parents x_1";
//        String formula = "x_1 spouse x_2 x_2 gender female x_1 gender male";
//        String formula = "x_1 spouse x_2 x_2 children x_3 x_1 children x_3";


    }
}
