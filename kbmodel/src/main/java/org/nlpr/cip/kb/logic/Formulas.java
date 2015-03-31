package org.nlpr.cip.kb.logic;

import com.google.common.base.Joiner;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.google.common.io.Files;
import org.apache.log4j.Logger;
import org.nlpr.cip.kb.util.SparqlEndpoint;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Set;

/**
 * User: hesz
 * Date: 2015-3-31
 * Time: 10:45
 */
public class Formulas {
    private static Logger log = Logger.getLogger(Formulas.class);
    public static SparqlEndpoint endpoint;
    public static String basePath = "G:\\temp\\TransX\\fb15k_380\\";


    public static void main(String[] args) throws IOException {
        String host = "172.18.28.117";//主机地址:172.18.28.117
        int port = 5003;//端口号:3039
        endpoint = SparqlEndpoint.getSparqlEndpoint(host, port);

        List<Relation> relations = Lists.newArrayList();
        for(String rel : Files.readLines(new File(basePath + "data\\relations.txt"), Charset.forName("utf-8"))){
            relations.add(new Relation(rel));
            relations.add(new Relation(rel, true));
        }
        System.out.println(relations.size());


        //一阶关系
//        List<String> formula_one = Lists.newArrayList();
//        for(int ind_one = 0; ind_one < relations.size(); ind_one ++){
//            Relation rel_one = relations.get(ind_one);
//            for(int ind_two = 0; ind_two < relations.size(); ind_two ++){
//                if(ind_two % 2 == 1) continue;
//                if(ind_two == ind_one) continue;
//                if(ind_one % 2 == 0 && ind_one + 1 == ind_two) continue;
//                if(ind_one % 2 == 1 && ind_one - 1 == ind_two) continue;
//                Relation rel_two = relations.get(ind_two);
//                Formula formula = new Formula(Arrays.asList(rel_one), rel_two);
//                if(formula.support() > 100 && formula.confidence() > 0.7){
//                    formula_one.add(formula.toSaveString());
////                    System.out.println(formula.toSaveString());
//                }
//            }
//            if(ind_one % 10 == 0) System.out.println(ind_one + ":" + new Date().toString());
//        }
//        System.out.println("one: " + formula_one.size());
//        String one_formula_path ="G:\\temp\\TransX\\fb15k_380\\data\\formulas\\one_formulas.txt";
//        BufferedWriter init_writer = Files.newWriter(new File(one_formula_path), Charset.forName("utf-8"));
//        init_writer.write(Joiner.on("\n").join(formula_one));
//        init_writer.close();

        //二阶关系
        Set<String> formula_two_set = Sets.newHashSet();
        for(int ind_one = 0; ind_one < relations.size(); ind_one ++) {
            Relation rel_one = relations.get(ind_one);
            for (int ind_two = 0; ind_two < relations.size(); ind_two++) {
                if (ind_two == ind_one) continue;
                if(ind_one % 2 == 0 && ind_one + 1 == ind_two) continue;
                if(ind_one % 2 == 1 && ind_one - 1 == ind_two) continue;
                Relation rel_two = relations.get(ind_two);
                List<Relation> bodies = Lists.newArrayList();
                bodies.add(rel_one);
                bodies.add(rel_two);
                if(new Formula().setBody(bodies).countBody() < 100) continue;

                for(int ind_three = 0; ind_three < relations.size(); ind_three ++) {
                    if(ind_three % 2 == 1) continue;
                    if (ind_three == ind_one || ind_three == ind_two) continue;
                    if (ind_three % 2 == 0 && (ind_three + 1 == ind_one || ind_three + 1 == ind_two)) continue;

                    Relation head = relations.get(ind_three);
                    Formula formula = new Formula(bodies, head);
                    String form_str = formula.toString();
                    if(formula_two_set.contains(form_str)) continue;
                    formula_two_set.add(form_str);
                }
            }
            System.out.println(ind_one + ":" + new Date().toString());
        }
        System.out.println("all two: " + formula_two_set.size());
        int validCount = 0;
        String two_formula_path ="G:\\temp\\TransX\\fb15k_380\\data\\formulas\\two_formulas.txt";
        BufferedWriter two_writer = Files.newWriter(new File(two_formula_path), Charset.forName("utf-8"));
        for(String form_str : formula_two_set){
            Formula formula = new Formula(form_str);
            if(formula.support() > 100 && formula.confidence() > 0.7) {
                two_writer.write(formula.toSaveString());
                two_writer.newLine();
                validCount++;
            }
        }
        two_writer.close();
        System.out.println("valid two: " + validCount);

    }
}


class Formula{
    public List<Relation> bodies = Lists.newArrayList();
    public Relation head;
    public long bodyCount = -1, headCount = -1, formulaCount = -1;

    public Formula(){}
    public Formula setBody(List<Relation> bodies){this.bodies = bodies; return this;}
    public Formula setHead(Relation head){this.head = head; return this;}
    public Formula(List<Relation> bodies, Relation head){
        this.bodies = bodies;
        this.head = head;
    }
    public Formula(String formula){this(formula.split("\t"));}
    public Formula(String[] atoms){this(Arrays.asList(atoms));}
    public Formula(List<String> atoms){
        for(int i = 0; i < atoms.size() - 1; i ++)
            bodies.add(new Relation(atoms.get(i)));
        head = new Relation(atoms.get(atoms.size() - 1));
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        for(Relation rel : bodies)
            sb.append(rel.toString() + "\t");
        sb.append(head.toString());
        return sb.toString();
    }

    public String toSaveString(){
        StringBuilder sb = new StringBuilder();
        for(Relation rel : bodies)
            sb.append(rel.toString() + "\t");
        sb.append(head.toString());
        sb.append(String.format("\n%d\t%d\t%d", countBody(), countHead(), count()));
        return sb.toString();
    }

    public long countBody(){
        if(bodyCount == -1){
            StringBuilder sparql = new StringBuilder();
            sparql.append("SELECT COUNT(*) WHERE \n{\n");
            int index = 0;
            for(Relation rel : bodies){
                String var1 = "?X_" + index++;
                String var2 = "?X_" + index;
                if(rel.isRev){String temp = var1;var1 = var2;var2 = temp;}
                sparql.append(String.format("%s\t<%s>\t%s\t.\n", var1, rel.label, var2));
            }
            sparql.append("}\n");
//            System.out.println(sparql.toString());
            bodyCount = Formulas.endpoint.count(sparql.toString());
        }
        return bodyCount;
    }

    public long countHead(){
        if(headCount == -1) {
            StringBuilder sparql = new StringBuilder();
            sparql.append("SELECT COUNT(*) WHERE \n{\n");
            if(head.isRev)
                sparql.append(String.format("%s\t<%s>\t%s\t.\n", "?X_0", head.label, "?X_1"));
            else
                sparql.append(String.format("%s\t<%s>\t%s\t.\n", "?X_1", head.label, "?X_0"));
            sparql.append("}\n");
//            System.out.println(sparql.toString());
            headCount = Formulas.endpoint.count(sparql.toString());
        }
        return headCount;
    }

    public long count(){
        if(formulaCount == -1) {
            StringBuilder sparql = new StringBuilder();
            sparql.append("SELECT COUNT(*) WHERE \n{\n");
            int index = 0;
            for (Relation rel : bodies) {
                String var1 = "?X_" + index++;
                String var2 = "?X_" + index;
                if (rel.isRev) { String temp = var1; var1 = var2; var2 = temp;}
                sparql.append(String.format("%s\t<%s>\t%s\t.\n", var1, rel.label, var2));
            }
            if(head.isRev)
                sparql.append(String.format("%s\t<%s>\t%s\t.\n", "?X_" + index, head.label, "?X_0"));
            else
                sparql.append(String.format("%s\t<%s>\t%s\t.\n", "?X_0", head.label, "?X_" + index));
            sparql.append("}\n");
//            System.out.println(sparql.toString());
            formulaCount = Formulas.endpoint.count(sparql.toString());
        }
        return formulaCount;
    }

    //支持度
    public long support(){
        return count();
    }

    //执行度
    public double confidence(){
        return count() * 1.0 / countBody();
    }


}
class Relation{
    public boolean isRev = false;
    public String label;
    public Relation(String label){
        if(label.endsWith("-1")) {
            this.isRev = true;
            this.label = label.substring(0, label.length() - 2);
        }
        else this.label = label;
    }
    public Relation(String label, boolean isRev){this.label = label; this.isRev = isRev;}
    public Relation rev(){return new Relation(label, !isRev);}

    public String toString(){
        if(isRev)
            return label + "-1";
        else
            return label;
    }
}
