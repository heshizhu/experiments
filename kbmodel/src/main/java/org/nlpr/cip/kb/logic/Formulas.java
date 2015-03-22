package org.nlpr.cip.kb.logic;

import com.google.common.base.Joiner;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.google.common.io.Files;
import org.apache.log4j.Logger;
import org.nlpr.cip.kb.util.SparqlEndpoint;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.*;

/**
 * User: hesz
 * Date: 2015-3-20
 * Time: 12:21
 */
public class Formulas {
    private static Logger log = Logger.getLogger(Formulas.class);
    public static SparqlEndpoint endpoint;
    public static String prefix = "?X_";//变量前缀，variable prefix


    //两个公式可能组合成的所有公式
    public static List<Formula> join(Formula one, Formula two){
        if(one.vars.size() < two.vars.size()){
            Formula temp = one;
            one = two; two = temp;
        }//保证two为变量少的那一个
        two.addVar(one.vars.size());
        List<String> cands = Lists.newArrayList();
        for(int i = 0;i < one.vars.size(); i ++)
            cands.add(one.vars.get(i));
        cands.add("NULL");

        List<Formula> new_formulas = Lists.newArrayList();
        List<String> vars = Lists.newArrayList();
        for(int i = 0; i < two.vars.size(); i ++)
            vars.add(two.vars.get(i));
        Set<String> var_match_results = search(vars, 0, two.vars.size(), cands);
        for(String matchRule : var_match_results){
            Map<String, String> matcher = Maps.newHashMap();
            String[] matchTemp = matchRule.split("\t");
            for(int i = 0; i < matchTemp.length; i ++){
                if(two.vars.get(i).equals(matchTemp[i]))
                    continue;
                matcher.put(two.vars.get(i), matchTemp[i]);
            }
            if(matcher.size() == 0) continue;
            List<Triple> join_atoms = Lists.newArrayList();
            for(Triple one_tri : one.atoms)
                join_atoms.add(new Triple(one_tri));
            for(Triple two_tri : two.atoms){
                join_atoms.add(two_tri.replaceVar(matcher));
            }
            Formula new_form = new Formula().setAtoms(join_atoms).strip();
            new_formulas.add(new_form);
        }

        return new_formulas;
    }

    private static Set<String> search(List<String> vars, int index, int count,
                                      List<String> cands){
        Set<String> full_res = Sets.newHashSet();
        Set<String> used = Sets.newHashSet();
        for(int i = 0; i < index; i ++)
            used.add(vars.get(i));
        for(String v : cands){
            String old = vars.get(index);
            if(!v.equals("NULL")){
                if(used.contains(v)) continue;
                vars.set(index, v);
            }
            if(index == (count - 1))
                full_res.add(Joiner.on("\t").join(vars));
            else
                full_res.addAll(search(vars, index + 1, count, cands));
            vars.set(index, old);//复原
        }
        return full_res;
    }

    public static void main(String[] args) throws IOException{
        String host = "172.18.28.117";//主机地址:172.18.28.117
        int port = 5002;//端口号:3039
        endpoint = SparqlEndpoint.getSparqlEndpoint(host, port);

//        buildInitFormula();

        List<Formula> one_formulas = Lists.newArrayList();
        String one_formula_path = "G:\\temp\\TransX\\fb15k\\data\\formulas\\one_formulas.txt";
        List<String> one_formuls_lines = Files.readLines(new File(one_formula_path), Charset.forName("utf-8"));
        for(int id = 0; id < one_formuls_lines.size(); id += 2)
            one_formulas.add(new Formula(one_formuls_lines.get(id).split("\t")));

        System.out.println("#one : " + one_formulas.size());
        //形成包含两个项的公式
        Set<String> two_formlulas_str = Sets.newHashSet();
        for(int outID = 0; outID < one_formulas.size(); outID ++){
            Formula one = one_formulas.get(outID);
            for(int inID = outID + 1; inID < one_formulas.size(); inID ++){
                Formula two = one_formulas.get(inID);
                for(Formula join_one : join(one, two))
                    two_formlulas_str.add(Joiner.on("\t").join(join_one.toSimpleString()));
            }
            if(outID % 1000 == 0) System.out.println(new Date() + ". deal with : " + outID);
        }
        System.out.println("二元总共：" + two_formlulas_str.size());

        int num = 0, two_num = 0, two_valid = 0;
        String two_formula_path = "G:\\temp\\TransX\\fb15k\\data\\formulas\\two_formulas.txt";
        BufferedWriter two_writer = Files.newWriter(new File(two_formula_path), Charset.forName("utf-8"));
        for(String form_str : two_formlulas_str){
            Formula formula = new Formula(form_str.split("\t"));
            int count = formula.count();
            if(num ++ % 1000 == 0) System.out.println((num - 1) + ":" + new Date());
            if(count > 0)
                two_num++;
            if(count > 10) {
                two_valid++;
                two_writer.write(Joiner.on("\t").join(formula.toSimpleString()));
                two_writer.newLine();
                two_writer.write(count);
                two_writer.newLine();
            }
        }
        two_writer.close();
        System.out.println("总共公式：" + two_num);
        System.out.println("有效公式：" + two_valid);


        //形成三个项的公式
//        List<Formula> three_formulas = Lists.newArrayList();
//        String three_formula_path = "G:\\temp\\TransX\\fb15k\\data\\formulas\\two_formulas.txt";

//        Formula one = new Formula("?X_0 /people/person/spouse_s./people/marriage/spouse ?X_1 ?X_0 /people/person/gender ?X_2".split(" "));
//        Formula two = new Formula("?X_0 /people/person/gender ?X_1".split(" "));
//
//        System.out.println(one.count());
//        System.out.println(two.count());
//
//        List<Formula> formulas = join(two, one);
//        for(Formula form : formulas){
//            if(form.count() <= 0) continue;
//            Formula join_one = form;
//            System.out.println(join_one + ":" + join_one.count());
//            List<String> join_str = join_one.toSimpleString();
//            Formula join_str_formula = new Formula(join_str);
//            System.out.println(join_str_formula + ":" + join_str_formula.count());
//            break;
//        }
    }

    public static void buildInitFormula() throws IOException {
        //收集原始公式
        String trainPath = "G:\\temp\\TransX\\fb15k\\data\\train.txt";
        Set<String> formula_str_trains = Sets.newHashSet();

//        for(String line : Files.readLines(new File(trainPath), Charset.forName("utf-8"))){
//            String[] terms = line.split("\t");
//            for(int id_1 = 0; id_1 < 2; id_1 ++) {
//                String term_0 = terms[0];
//                if(id_1 == 0)
//                    terms[0] = prefix + 0;
//                for (int id_2 = 0; id_2 < 2; id_2++){
//                    String term_1 = terms[1];
//                    if(id_2 == 0)
//                        terms[1] = prefix + 1;
//                    for (int id_3 = 0; id_3 < 2; id_3++) {
//                        String term_2 = terms[2];
//                        if(id_1 == 0 && id_2 == 0 && id_3 == 0) continue;
//                        if(id_3 == 0)
//                            terms[2] = prefix + 2;
//                        formula_str_trains.add(Joiner.on("\t").join(terms));
//                        terms[2] = term_2;
//                    }
//                    terms[1] = term_1;
//                }
//                terms[0] = term_0;
//            }
//        }
        //保证关系不为变量
        for(String line : Files.readLines(new File(trainPath), Charset.forName("utf-8"))){
            String[] terms = line.split("\t");
            for(int id_1 = 0; id_1 < 2; id_1 ++) {
                String term_0 = terms[0];
                if(id_1 == 0)
                    terms[0] = prefix + 0;
                for (int id_3 = 0; id_3 < 2; id_3++) {
                    String term_2 = terms[2];
                    if(id_1 == 0 && id_3 == 0) continue;
                    if(id_3 == 0)
                        terms[2] = prefix + 2;
                    formula_str_trains.add(Joiner.on("\t").join(terms));
                    terms[2] = term_2;
                }
                terms[0] = term_0;
            }
        }
        System.out.println(formula_str_trains.size());
        System.out.println("过滤过前：" + formula_str_trains.size());

        Set<String> form_str_filter = Sets.newHashSet();
        for(String form_str : formula_str_trains) {
            Formula formula = new Formula(form_str.split("\t"));
            form_str_filter.add(Joiner.on("\t").join(formula.toSimpleString()));
        }
        System.out.println("过滤过后：" + form_str_filter.size());

        String one_formula_path ="G:\\temp\\TransX\\fb15k\\data\\formulas\\init_formulas.txt";
        BufferedWriter init_writer = Files.newWriter(new File(one_formula_path), Charset.forName("utf-8"));
        int num = 0, init_num = 0;
        for(String form_str : form_str_filter){
            Formula formula = new Formula(form_str.split("\t"));
            int count = formula.count();
            if(num ++ % 1000 == 0) System.out.println((num - 1) + ":" + new Date());
            if(count > 10) {
                init_writer.write(Joiner.on("\t").join(formula.toSimpleString()));
                init_writer.newLine();
                init_writer.write(String.format("%d", count));
                init_writer.newLine();
                init_num++;
            }
        }
        init_writer.close();
        System.out.println("有效个数：" + init_num);
    }
}

class Formula{
    public List<String> vars = Lists.newArrayList();
    public List<Triple> atoms = Lists.newArrayList();

    public Formula(){}
    //?X_0 child ?X_1 ?X_1 child ?X_2
    //?X_0 grandson ?X_1
    public Formula(String... terms){
        List<String> new_terms = Lists.newArrayList();
        if(terms.length == 0){

        }
        else {
            for (String temp : terms)
                new_terms.add(temp);
            initFormula(new_terms);
        }
    }

    public Formula(List<String> terms){
        initFormula(terms);
    }

    public Formula setAtoms(List<Triple> that){
        this.atoms = that; return this;
    }

    //把变量加上一定的增量，如：X_1 2 => X_3
    private String varAddVal(String var, int val){
        int new_val = val + Integer.parseInt(var.substring(Formulas.prefix.length()));
        return Formulas.prefix + new_val;
    }

    //把每个变量名都加val
    public void addVar(int val){
        for(int i = 0; i < vars.size(); i ++){
            String var = vars.get(i);
            if(var.startsWith(Formulas.prefix))
                vars.set(i, varAddVal(var, val));
        }
        for(int i = 0; i < atoms.size(); i ++){
            Triple atom = atoms.get(i);
            for(int id = 0; id < atom.terms.length; id ++){
                String var = atom.terms[id];
                if(var.startsWith(Formulas.prefix))
                    atom.terms[id] = varAddVal(var, val);
            }
        }
    }

    public List<String> toSimpleString(){
        List<String> terms = Lists.newArrayList();
        for(Triple t : atoms)
            for(int i = 0; i < t.terms.length; i ++)
                terms.add(t.terms[i]);
        return terms;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("lambda " + Joiner.on(", ").join(vars) + " . ");
        List<String> temp = Lists.newArrayList();
        for(Triple t : atoms)
            temp.add(String.format("%s(%s, %s)", t.terms[1], t.terms[0], t.terms[2]));
        sb.append(Joiner.on(" & ").join(temp));
        return sb.toString();
    }

    private void initFormula(List<String> paras){
        Set<String> terms_set = Sets.newHashSet();
        for(int i = 0; i < paras.size(); i += 3){
            String[] terms = new String[3];
            for(int j = i; j < i + 3; j ++){
                String term = paras.get(j);
                terms[j - i] = term;
                if(term.startsWith(Formulas.prefix) && !terms_set.contains(term)){
                    vars.add(term);
                    terms_set.add(term);
                }
            }
            atoms.add(new Triple(terms));
        }
        strip();
    }


    //其在知识库中出下的次数
    public int count(){
        StringBuilder sparql = new StringBuilder();
        sparql.append("SELECT COUNT(*) WHERE \n{\n");
        for(Triple atom : atoms){
            for(int id = 0; id < atom.terms.length; id ++) {
                if (atom.terms[id].startsWith(Formulas.prefix))
                    sparql.append(atom.terms[id] + "\t");
                else
                    sparql.append("<" + atom.terms[id] + ">\t");
            }
            sparql.append(".\n");
        }

        sparql.append("}\n");
//        System.out.println(sparql.toString());
        return Formulas.endpoint.count(sparql.toString());
    }

    //对atoms中出现的变量名进行重新整理，按照其出现的先后顺序
    public Formula strip(){
        Map<String, String> var_id = Maps.newHashMap();
        for(Triple tri : atoms){
            for(int id = 0; id < tri.terms.length; id ++) {
                if(tri.terms[id].startsWith(Formulas.prefix) && !var_id.containsKey(tri.terms[id]))
                    var_id.put(tri.terms[id], Formulas.prefix + var_id.size());
            }
        }
        vars.clear();
        for(int i = 0; i < var_id.size(); i ++)
            vars.add(Formulas.prefix + i);
        for(Triple tri : atoms){
            for(int id = 0; id < tri.terms.length; id ++) {
                if(tri.terms[id].startsWith(Formulas.prefix))
                    tri.terms[id] = var_id.get(tri.terms[id]);
            }
        }
        return this;
    }
}