package org.nlpr.cip.kb.corpus;

import com.google.common.collect.Sets;
import org.apache.log4j.Logger;
import org.nlpr.cip.kb.util.SparqlEndpoint;

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

//        String rule = "x_1 children x_2 x_2 parents x_1";
//        String rule = "x_1 spouse x_2 x_2 gender female x_1 gender male";
        String rule = "x_1 spouse x_2 x_2 children x_3 x_1 children x_3";

        String[] terms = rule.split(" ");
        if(terms.length % 3 != 0){
            System.out.println("input error: not a valid rule");
            System.exit(0);
        }


        //获得规则的支持频率和置信度
        //获得自由变量
        Set<String> free_variables = Sets.newHashSet();
        for(String term : terms){
            if(term.startsWith("x_"))
                free_variables.add(term);
        }
        int variable_num = free_variables.size();

        StringBuilder sparql = new StringBuilder();
        sparql.append("SELECT COUNT( * )WHERE {\n");
        for(int i = 0; i < terms.length; i += 3){
            String sub = terms[i];
            String rel = terms[i + 1];
            String obj = terms[i + 2];
            if(sub.startsWith("x_"))
                sub = "?" + sub;
            else
                sub = "<" + sub + ">";
            if(rel.startsWith("x_"))
                rel = "?" + rel;
            else
                rel = "<" + rel + ">";
            if(obj.startsWith("x_"))
                obj = "?" + obj;
            else
                obj = "<" + obj + ">";
            String triple = String.format("%s\t%s\t%s .\n", sub, rel, obj);
            sparql.append(triple);
        }
        sparql.append("}");
        System.out.println(sparql);
        System.out.println(endpoint.count(sparql.toString()));
    }
}
