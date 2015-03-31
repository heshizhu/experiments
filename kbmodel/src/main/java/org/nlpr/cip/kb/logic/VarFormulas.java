package org.nlpr.cip.kb.logic;

import com.google.common.base.Joiner;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.google.common.io.Files;
import org.apache.log4j.Logger;
import org.nlpr.cip.kb.util.CollectionUtils;
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
public class VarFormulas {
    private static Logger log = Logger.getLogger(VarFormulas.class);
    public static SparqlEndpoint endpoint;
    public static String prefix = "?X_";//变量前缀，variable prefix

    //两个公式可能组合成的所有公式
    public static List<VarFormula> join(VarFormula one, VarFormula two){
        one = new VarFormula(one.toSimpleString());
        two = new VarFormula(two.toSimpleString());

        if(one.vars.size() < two.vars.size()){
            VarFormula temp = one;
            one = two; two = temp;
        }//保证two为变量少的那一个
        two.addVar(one.vars.size());
        List<String> cands = Lists.newArrayList();
        for(int i = 0;i < one.vars.size(); i ++)
            cands.add(one.vars.get(i));
        cands.add("NULL");

        List<VarFormula> new_VarFormulas = Lists.newArrayList();
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
            VarFormula new_form = new VarFormula().setAtoms(join_atoms).strip();
            new_VarFormulas.add(new_form);
        }

        return new_VarFormulas;
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
        int port = 5003;//端口号:3039
        endpoint = SparqlEndpoint.getSparqlEndpoint(host, port);

//        System.out.println("begin one way.");
//        buildInitVarFormula();
        System.out.println("begin two way.");
        buildTwoWayVarFormula();
        System.out.println("begin three way.");
        buildThreeWayVarFormula();
//        System.out.println("all over.");

//        collectLogicalRules();//收集高频逻辑规则

//        test();
    }

    public static void collectLogicalRules() throws IOException{
        List<VarFormula> one_VarFormulas = Lists.newArrayList();
        Map<String, Long> one_VarFormula_frequent = Maps.newHashMap();
        String one_VarFormula_path = "G:\\temp\\TransX\\fb15k\\data\\VarFormulas\\one_VarFormulas.txt";
        List<String> one_formuls_lines = Files.readLines(new File(one_VarFormula_path), Charset.forName("utf-8"));
        for(int id = 0; id < one_formuls_lines.size(); id += 2){
            String one_str = one_formuls_lines.get(id);
            VarFormula one = new VarFormula(one_str.split("\t"));
            long frq = Long.parseLong(one_formuls_lines.get(id + 1));
            one_VarFormulas.add(one);
            one_VarFormula_frequent.put(one_str, frq);
        }

        List<VarFormula> two_VarFormulas = Lists.newArrayList();
        Map<String, Long> two_VarFormula_frequent = Maps.newHashMap();
        String two_VarFormula_path = "G:\\temp\\TransX\\fb15k\\data\\VarFormulas\\two_VarFormulas.txt";
        List<String> two_formuls_lines = Files.readLines(new File(two_VarFormula_path), Charset.forName("utf-8"));
        for(int id = 0; id < two_formuls_lines.size(); id += 2) {
            String two_str = two_formuls_lines.get(id);
            VarFormula two = new VarFormula(two_str.split("\t"));
            long frq = Long.parseLong(two_formuls_lines.get(id + 1));
            two_VarFormulas.add(two);
            two_VarFormula_frequent.put(two_str, frq);
        }

        List<VarFormula> three_VarFormulas = Lists.newArrayList();
        Map<String, Long> three_VarFormula_frequent = Maps.newHashMap();
        String three_VarFormula_path = "G:\\temp\\TransX\\fb15k\\data\\VarFormulas\\three_VarFormulas.txt";
        List<String> three_formuls_lines = Files.readLines(new File(three_VarFormula_path), Charset.forName("utf-8"));
        for(int id = 0; id < three_formuls_lines.size(); id += 2) {
            String three_str = three_formuls_lines.get(id);
            VarFormula three = new VarFormula(three_str.split("\t"));
            long frq = Long.parseLong(three_formuls_lines.get(id + 1));
            three_VarFormulas.add(three);
            three_VarFormula_frequent.put(three_str, frq);
        }

        //发现二阶推理规则
        Map<String, Double> body_head_conf = Maps.newHashMap();
        Map<String, Double> two_VarFormula_conf = Maps.newHashMap();
        for(int outID = 0; outID < one_VarFormulas.size(); outID ++){
            VarFormula one = one_VarFormulas.get(outID);
            String one_str = Joiner.on("\t").join(one.toSimpleString());
            long one_frq = one_VarFormula_frequent.get(one_str);
            for(int inID = outID + 1; inID < one_VarFormulas.size(); inID ++){
                VarFormula two = one_VarFormulas.get(inID);
                String two_str = Joiner.on("\t").join(two.toSimpleString());
                long two_frq = one_VarFormula_frequent.get(two_str);
                for(VarFormula join_one : join(one, two)){
                    String join_str = Joiner.on("\t").join(join_one.toSimpleString());
                    if(!two_VarFormula_frequent.containsKey(join_str)) continue;
                    long join_frq = two_VarFormula_frequent.get(join_str);
                    double one_infer_two = 1.0 * join_frq / one_frq;
                    double two_infer_one = 1.0 * join_frq / two_frq;

                    double max_conf = one_infer_two;
                    if(two_infer_one > one_infer_two) max_conf = two_infer_one;
                    two_VarFormula_conf.put(join_str, max_conf);

                    // one => two 的置信度为 one_infer_two
                    // two => one 的置信度为 two_infer_one
                    body_head_conf.put(String.format("%s\n%s", one_str, two_str), one_infer_two);
                    body_head_conf.put(String.format("%s\n%s", two_str, one_str), two_infer_one);
                }
            }
        }
//        String two_VarFormula_filter_path = "G:\\temp\\TransX\\fb15k\\data\\VarFormulas\\two_VarFormulas_1000.txt";
//        BufferedWriter two_filter_writer = Files.newWriter(new File(two_VarFormula_filter_path), Charset.forName("utf-8"));
//        List<Map.Entry<String, Double>> two_filter_entries = (List<Map.Entry<String, Double>>)CollectionUtils.sortMapByValueDESC(two_VarFormula_conf);
//        for(int i = 0; i < two_filter_entries.size() && i < 1000; i ++){
//            Map.Entry<String, Double> entry = two_filter_entries.get(i);
//            String VarFormula = entry.getKey();
//            int frq = two_VarFormula_frequent.get(VarFormula);
//            two_filter_writer.write(String.format("%s\n%d\n", VarFormula, frq));
//        }
//        two_filter_writer.close();

        //对value进行排序
        String two_rule_path = "G:\\temp\\TransX\\fb15k\\data\\VarFormulas\\two_rules.txt";
        BufferedWriter two_rule_writer = Files.newWriter(new File(two_rule_path), Charset.forName("utf-8"));
        List<Map.Entry<String, Double>> two_order_entries = (List<Map.Entry<String, Double>>)CollectionUtils.sortMapByValueDESC(body_head_conf);
        for(Map.Entry<String, Double> entry : two_order_entries){
            String[] keys = entry.getKey().split("\n");
            String one_str = keys[0];
            String two_str = keys[1];
            long one_frq = one_VarFormula_frequent.get(one_str);
            long two_frq = one_VarFormula_frequent.get(two_str);
            double conf = entry.getValue();
            two_rule_writer.write(one_str);
            two_rule_writer.newLine();
            two_rule_writer.write(two_str);
            two_rule_writer.newLine();
            two_rule_writer.write(String.format("%d\n%d\n%f", one_frq, two_frq, conf));
            two_rule_writer.newLine();
        }
        two_rule_writer.close();


        //发现三阶推理规则
        body_head_conf = Maps.newHashMap();
        for(int outID = 0; outID < two_VarFormulas.size(); outID ++){
            VarFormula two = two_VarFormulas.get(outID);
            String two_str = Joiner.on("\t").join(two.toSimpleString());
            long two_frq = two_VarFormula_frequent.get(two);
            for(int inID = outID + 1; inID < one_VarFormulas.size(); inID ++){
                VarFormula one = one_VarFormulas.get(inID);
                String one_str = Joiner.on("\t").join(one.toSimpleString());
                long one_frq = one_VarFormula_frequent.get(two);
                for(VarFormula join : join(two, one)){
                    String join_str = Joiner.on("\t").join(join.toSimpleString());
                    if(!three_VarFormula_frequent.containsKey(join_str)) continue;
                    long join_frq = two_VarFormula_frequent.get(join_str);
                    double two_infer_one = 1.0 * join_frq / two_frq;
                    // two => one 的置信度为 two_infer_one
                    body_head_conf.put(String.format("%s\n%s", two_str, one_str), two_infer_one);
                }
            }
        }
        //对value进行排序
        String three_rule_path = "G:\\temp\\TransX\\fb15k\\data\\VarFormulas\\init_three_rules.txt";
        BufferedWriter three_rule_writer = Files.newWriter(new File(three_rule_path), Charset.forName("utf-8"));
        List<Map.Entry<String, Double>> three_order_entries = (List<Map.Entry<String, Double>>)CollectionUtils.sortMapByValueDESC(body_head_conf);
        for(Map.Entry<String, Double> entry : three_order_entries){
            String[] keys = entry.getKey().split("\n");
            String two_str = keys[0];
            String one_str = keys[1];
            long two_frq = two_VarFormula_frequent.get(two_str);
            long one_frq = one_VarFormula_frequent.get(one_str);
            double conf = entry.getValue();
            three_rule_writer.write(two_str);
            three_rule_writer.newLine();
            three_rule_writer.write(one_str);
            three_rule_writer.newLine();
            three_rule_writer.write(String.format("%d\n%d\n%f", two_frq, one_frq, conf));
            three_rule_writer.newLine();
        }
        three_rule_writer.close();
    }


    public static void test(){
        String line = "?X_0\t/organization/organization_member/member_of./organization/organization_membership/organization\t/m/02vk52z";
        VarFormula one = new VarFormula(line.split("\t"));
        String one_str = Joiner.on("\t").join(one.toSimpleString());
        System.out.println(one_str);
        System.out.println(Joiner.on("\t").join(one.toSimpleString()));
//        VarFormula one = new VarFormula("?X_0 /people/person/spouse_s./people/marriage/spouse ?X_1 ?X_0 /people/person/gender ?X_2".split(" "));
//        VarFormula two = new VarFormula("?X_0 /people/person/gender ?X_1".split(" "));
//
//        System.out.println(one.count());
//        System.out.println(two.count());
//
//        List<VarFormula> VarFormulas = join(two, one);
//        for(VarFormula form : VarFormulas){
//            if(form.count() <= 0) continue;
//            VarFormula join_one = form;
//            System.out.println(join_one + ":" + join_one.count());
//            List<String> join_str = join_one.toSimpleString();
//            VarFormula join_str_VarFormula = new VarFormula(join_str);
//            System.out.println(join_str_VarFormula + ":" + join_str_VarFormula.count());
//            break;
//        }
    }

    public static void buildThreeWayVarFormula() throws IOException{
        List<VarFormula> one_VarFormulas = Lists.newArrayList();
        Map<VarFormula, Integer> one_VarFormula_frequent = Maps.newHashMap();
        String one_VarFormula_path = "G:\\temp\\TransX\\fb15k_380\\data\\VarFormulas\\one_VarFormulas.txt";
        List<String> one_formuls_lines = Files.readLines(new File(one_VarFormula_path), Charset.forName("utf-8"));
        for(int id = 0; id < one_formuls_lines.size(); id += 2){
            VarFormula one = new VarFormula(one_formuls_lines.get(id).split("\t"));
            int frq = Integer.parseInt(one_formuls_lines.get(id + 1));
            one_VarFormulas.add(one);
            one_VarFormula_frequent.put(one, frq);
        }


        List<VarFormula> two_VarFormulas = Lists.newArrayList();
        Map<VarFormula, Integer> two_VarFormula_frequent = Maps.newHashMap();
        String two_VarFormula_path = "G:\\temp\\TransX\\fb15k_380\\data\\VarFormulas\\two_VarFormulas.txt";
        List<String> two_formuls_lines = Files.readLines(new File(two_VarFormula_path), Charset.forName("utf-8"));
        for(int id = 0; id < two_formuls_lines.size(); id += 2) {
            VarFormula two = new VarFormula(two_formuls_lines.get(id).split("\t"));
            int frq = Integer.parseInt(two_formuls_lines.get(id + 1));
            two_VarFormulas.add(two);
            two_VarFormula_frequent.put(two, frq);
        }

        System.out.println("#one : " + one_VarFormulas.size());
        System.out.println("#two : " + two_VarFormulas.size());

        int three_src_num = 0, three_num = 0, three_valid = 0;
        String three_VarFormula_path = "G:\\temp\\TransX\\fb15k_380\\data\\VarFormulas\\three_VarFormulas.txt";
        BufferedWriter three_writer = Files.newWriter(new File(three_VarFormula_path), Charset.forName("utf-8"));
        for(int outID = 0; outID < two_VarFormulas.size(); outID ++){
            Set<String> three_formlulas_str = Sets.newHashSet();
            VarFormula two = two_VarFormulas.get(outID);
            int twoFrq = two_VarFormula_frequent.get(two);
            for(int inID = 0; inID < one_VarFormulas.size(); inID ++){
                VarFormula one = one_VarFormulas.get(inID);
                int oneFrq = one_VarFormula_frequent.get(one);
                for(VarFormula join_one : join(two, one)){
                    three_src_num ++;
                    String join_str = Joiner.on("\t").join(join_one.toSimpleString());
                    if(three_formlulas_str.contains(join_str)) continue;
                    three_formlulas_str.add(join_str);
                    long join_frq = join_one.count();
                    if(join_frq > 0) three_num++;
                    if(join_frq <= 100) continue;
                    double one_infer_join = 1.0 * join_frq / oneFrq;
                    double two_infer_join = 1.0 * join_frq / twoFrq;
                    //有效候选规则
                    if(one_infer_join > 0.7 || two_infer_join > 0.7){
                        three_valid++;
                        three_writer.write(join_str);
                        three_writer.newLine();
                        three_writer.write(String.format("%d", join_frq));
                        three_writer.newLine();
                    }
                }
            }
            if(outID % 100 == 0) System.out.println(new Date() + ". deal with : " + outID);
        }

        three_writer.close();

        System.out.println("三元总共：" + three_src_num);
        System.out.println("成立总共：" + three_num);
        System.out.println("有效总共：" + three_valid);
    }

    public static void buildTwoWayVarFormula() throws IOException{
        List<VarFormula> one_VarFormulas = Lists.newArrayList();
        Map<VarFormula, Integer> one_VarFormula_frequent = Maps.newHashMap();
        String one_VarFormula_path = "G:\\temp\\TransX\\fb15k_380\\data\\VarFormulas\\one_VarFormulas.txt";
        List<String> one_formuls_lines = Files.readLines(new File(one_VarFormula_path), Charset.forName("utf-8"));
        for(int id = 0; id < one_formuls_lines.size(); id += 2){
            VarFormula VarFormula = new VarFormula(one_formuls_lines.get(id).split("\t"));
            int frq = Integer.parseInt(one_formuls_lines.get(id + 1));
            one_VarFormulas.add(VarFormula);
            one_VarFormula_frequent.put(VarFormula, frq);
        }
        System.out.println("#one : " + one_VarFormulas.size());

        //形成包含两个项的公式
        int two_src_num = 0, two_num = 0, two_valid = 0;
        String two_VarFormula_path = "G:\\temp\\TransX\\fb15k_380\\data\\VarFormulas\\two_VarFormulas.txt";
        BufferedWriter two_writer = Files.newWriter(new File(two_VarFormula_path), Charset.forName("utf-8"));
        for(int outID = 0; outID < one_VarFormulas.size(); outID ++){
            Set<String> two_formlulas_str = Sets.newHashSet();
            VarFormula one = one_VarFormulas.get(outID);
            long oneFrq = one_VarFormula_frequent.get(one);
            for(int inID = outID + 1; inID < one_VarFormulas.size(); inID ++){
                VarFormula two = one_VarFormulas.get(inID);
                long twoFrq = one_VarFormula_frequent.get(two);
                for(VarFormula join_one : join(one, two)){
                    two_src_num ++;
                    String join_one_str = Joiner.on("\t").join(join_one.toSimpleString());
                    if(two_formlulas_str.contains(join_one_str)) continue;
                    two_formlulas_str.add(join_one_str);

                    long join_frq = join_one.count();
                    if(join_frq > 0) two_num ++;
                    if(join_frq <= 100) continue;
                    double one_infer_join = 1.0 * join_frq / oneFrq;
                    double two_infer_join = 1.0 * join_frq / twoFrq;
                    //有效候选规则
                    if(one_infer_join > 0.7 || two_infer_join > 0.7){
                        two_valid++;
                        two_writer.write(join_one_str);
                        two_writer.newLine();
                        two_writer.write(String.format("%d", join_frq));
                        two_writer.newLine();
                    }

                }
            }
            if(outID % 100 == 0) System.out.println(new Date() + ". deal with : " + outID);
        }
        two_writer.close();
        System.out.println("二元总共：" + two_src_num);
        System.out.println("成立总共：" + two_num);
        System.out.println("有效总共：" + two_valid);
    }

    public static void buildInitVarFormula() throws IOException {
        //收集原始公式
        String trainPath = "G:\\temp\\TransX\\fb15k_380\\data\\train.txt";
        Set<String> VarFormula_str_trains = Sets.newHashSet();

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
//                        VarFormula_str_trains.add(Joiner.on("\t").join(terms));
//                        terms[2] = term_2;
//                    }
//                    terms[1] = term_1;
//                }
//                terms[0] = term_0;
//            }
//        }

        //关系不为变量，前后变量
        for(String line : Files.readLines(new File(trainPath), Charset.forName("utf-8"))){
            String[] terms = line.split("\t");
            terms[0] = prefix + 0;
            terms[2] = prefix + 1;
            VarFormula_str_trains.add(Joiner.on("\t").join(terms));
//            for(int id_1 = 0; id_1 < 2; id_1 ++) {
////                String term_0 = terms[0];
//                if(id_1 == 0)
//                    terms[0] = prefix + 0;
//                for (int id_3 = 0; id_3 < 2; id_3++) {
//                    String term_2 = terms[2];
//                    if(id_3 == 0)
//                        terms[2] = prefix + 2;
//                    VarFormula_str_trains.add(Joiner.on("\t").join(terms));
//                    terms[2] = term_2;
//                }
////                terms[0] = term_0;
//            }
        }
        System.out.println(VarFormula_str_trains.size());
        System.out.println("过滤过前：" + VarFormula_str_trains.size());

        Set<String> form_str_filter = Sets.newHashSet();
        for(String form_str : VarFormula_str_trains) {
            VarFormula VarFormula = new VarFormula(form_str.split("\t"));
            form_str_filter.add(Joiner.on("\t").join(VarFormula.toSimpleString()));
        }
        System.out.println("过滤过后：" + form_str_filter.size());

        String one_VarFormula_path ="G:\\temp\\TransX\\fb15k_380\\data\\VarFormulas\\one_VarFormulas.txt";
        BufferedWriter init_writer = Files.newWriter(new File(one_VarFormula_path), Charset.forName("utf-8"));
        int num = 0, init_num = 0;
        for(String form_str : form_str_filter){
            VarFormula VarFormula = new VarFormula(form_str.split("\t"));
            long count = VarFormula.count();
            if(num ++ % 1000 == 0) System.out.println((num - 1) + ":" + new Date());
            if(count >= 100) {
                init_writer.write(Joiner.on("\t").join(VarFormula.toSimpleString()));
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

class VarFormula{
    public List<String> vars = Lists.newArrayList();
    public List<Triple> atoms = Lists.newArrayList();

    public VarFormula(){}
    //?X_0 child ?X_1 ?X_1 child ?X_2
    //?X_0 grandson ?X_1
    public VarFormula(String... terms){
        List<String> new_terms = Lists.newArrayList();
        if(terms.length == 0){

        }
        else {
            for (String temp : terms)
                new_terms.add(temp);
            initVarFormula(new_terms);
        }
    }

    public VarFormula(List<String> terms){
        initVarFormula(terms);
    }

    public VarFormula setAtoms(List<Triple> that){
        this.atoms = that; return this;
    }

    //把变量加上一定的增量，如：X_1 2 => X_3
    private String varAddVal(String var, int val){
        int new_val = val + Integer.parseInt(var.substring(VarFormulas.prefix.length()));
        return VarFormulas.prefix + new_val;
    }

    //把每个变量名都加val
    public void addVar(int val){
        for(int i = 0; i < vars.size(); i ++){
            String var = vars.get(i);
            if(var.startsWith(VarFormulas.prefix))
                vars.set(i, varAddVal(var, val));
        }
        for(int i = 0; i < atoms.size(); i ++){
            Triple atom = atoms.get(i);
            for(int id = 0; id < atom.terms.length; id ++){
                String var = atom.terms[id];
                if(var.startsWith(VarFormulas.prefix))
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

    private void initVarFormula(List<String> paras){
        Set<String> terms_set = Sets.newHashSet();
        for(int i = 0; i < paras.size(); i += 3){
            String[] terms = new String[3];
            for(int j = i; j < i + 3; j ++){
                String term = paras.get(j);
                terms[j - i] = term;
                if(term.startsWith(VarFormulas.prefix) && !terms_set.contains(term)){
                    vars.add(term);
                    terms_set.add(term);
                }
            }
            atoms.add(new Triple(terms));
        }
        strip();
    }


    //其在知识库中出下的次数
    public long count(){
        StringBuilder sparql = new StringBuilder();
        sparql.append("SELECT COUNT(*) WHERE \n{\n");
        for(Triple atom : atoms){
            for(int id = 0; id < atom.terms.length; id ++) {
                if (atom.terms[id].startsWith(VarFormulas.prefix))
                    sparql.append(atom.terms[id] + "\t");
                else
                    sparql.append("<" + atom.terms[id] + ">\t");
            }
            sparql.append(".\n");
        }

        sparql.append("}\n");
//        System.out.println(sparql.toString());
        return VarFormulas.endpoint.count(sparql.toString());
    }

    //对atoms中出现的变量名进行重新整理，按照其出现的先后顺序
    public VarFormula strip(){
        Map<String, String> var_id = Maps.newHashMap();
        for(Triple tri : atoms){
            for(int id = 0; id < tri.terms.length; id ++) {
                if(tri.terms[id].startsWith(VarFormulas.prefix) && !var_id.containsKey(tri.terms[id]))
                    var_id.put(tri.terms[id], VarFormulas.prefix + var_id.size());
            }
        }
        vars.clear();
        for(int i = 0; i < var_id.size(); i ++)
            vars.add(VarFormulas.prefix + i);
        for(Triple tri : atoms){
            for(int id = 0; id < tri.terms.length; id ++) {
                if(tri.terms[id].startsWith(VarFormulas.prefix))
                    tri.terms[id] = var_id.get(tri.terms[id]);
            }
        }
        return this;
    }
}