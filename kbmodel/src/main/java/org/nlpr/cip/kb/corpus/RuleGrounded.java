package org.nlpr.cip.kb.corpus;

import com.google.common.collect.Sets;
import org.apache.log4j.Logger;
import org.nlpr.cip.kb.util.SparqlEndpoint;

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


    public static void main(String[] args) {
        SparqlEndpoint endpoint = SparqlEndpoint.getSparqlEndpoint("172.18.28.117", 5001);

//        String formula = "x_1 children x_2 x_2 parents x_1";
//        String formula = "x_1 spouse x_2 x_2 gender female x_1 gender male";
        String formula = "x_1 spouse x_2 x_2 children x_3 x_1 children x_3";


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
        int supportFrq = endpoint.count(sparql.toString());


        //互动区其支持度
        sparql = new StringBuilder();
        sparql.append("SELECT COUNT( * )WHERE {\n");
        for(int i = 0; i < terms.length - 3; i += 3){
            String triple = String.format("%s\t%s\t%s .\n", terms[i], terms[i+1], terms[i+2]);
            sparql.append(triple);
        }
        sparql.append("}");
//        System.out.println(sparql.toString());
        int bodySupportFrq = endpoint.count(sparql.toString());
        double support = supportFrq * 1.0 / bodySupportFrq;

        System.out.println("frquent: " + supportFrq);
        System.out.println("(body)frquent: " + bodySupportFrq);
        System.out.println("support: " + support);


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
        System.out.println("size: " + (results.size() - 1));
        for(int i = 0; i < 10 && i < results.size(); i ++)
            System.out.println(results.get(i));
    }
}
