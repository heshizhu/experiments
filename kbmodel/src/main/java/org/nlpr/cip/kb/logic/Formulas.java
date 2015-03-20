package org.nlpr.cip.kb.logic;

import com.google.common.base.Joiner;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import org.apache.log4j.Logger;
import org.nlpr.cip.kb.util.SparqlEndpoint;

import java.text.Normalizer;
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
        List<Formula> new_formulas = Lists.newArrayList();
        List<String> vars = Lists.newArrayList();
        for(int i = 0; i < two.vars.size(); i ++)
            vars.add("temp_" + i);
        Set<String> var_match_results = search(vars, 0, two.vars.size(), one.vars);
        for(String matchRule : var_match_results){
            Map<String, String> matcher = Maps.newHashMap();
            String[] matchTemp = matchRule.split("\t");
            for(int i = 0; i < matchTemp.length; i ++)
                matcher.put(prefix + i, matchTemp[i]);
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
            if(used.contains(v)) continue;
            vars.set(index, v);
            if(index == (count - 1))
                full_res.add(Joiner.on("\t").join(vars));
            else
                full_res.addAll(search(vars, index + 1, count, cands));
        }
        return full_res;
    }

    public static void main(String[] args) {
        Formula one = new Formula("?X_0 child ?X_1 ?X_1 child ?X_2".split(" "));
        Formula two = new Formula("?X_0 grandson ?X_1".split(" "));
        System.out.println(one);
        System.out.println(two);

        List<Formula> formulas = join(two, one);
        for(Formula form : formulas)
            System.out.println(form);
    }
}

class Formula{
    public List<String> vars = Lists.newArrayList();
    public List<Triple> atoms = Lists.newArrayList();

    public Formula(){}
    public Formula setAtoms(List<Triple> that){
        this.atoms = that; return this;
    }

    //?X_0 child ?X_1 ?X_1 child ?X_2
    //?X_0 grandson ?X_1
    public Formula(String... terms){
        List<String> new_terms = Lists.newArrayList();
        for(String temp : terms)
            new_terms.add(temp);
        initFormula(new_terms);
    }

    public Formula(List<String> terms){
        initFormula(terms);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("lambda " + Joiner.on(", ").join(vars) + " . ");
        List<String> temp = Lists.newArrayList();
        for(Triple t : atoms)
            temp.add(String.format("%s(%s, %s)", t.r, t.h, t.t));
        sb.append(Joiner.on(" & ").join(temp));
        return sb.toString();
    }

    private void initFormula(List<String> terms){
        Set<String> terms_set = Sets.newHashSet();
        for(int i = 0; i < terms.size(); i += 3){
            String h = terms.get(i);
            if(h.startsWith(Formulas.prefix) && !terms_set.contains(h)){
                vars.add(h);
                terms_set.add(h);
            }
            String r = terms.get(i + 1);
            if(r.startsWith(Formulas.prefix) && !terms_set.contains(r)){
                vars.add(r);
                terms_set.add(r);
            }
            String t = terms.get(i + 2);
            if(t.startsWith(Formulas.prefix) && !terms_set.contains(t)){
                vars.add(t);
                terms_set.add(t);
            }
            atoms.add(new Triple(h, r, t));
        }
        strip();
    }


    //其在知识库中出下的次数
    public int count(){
        StringBuilder sparql = new StringBuilder();
        sparql.append("SELECT DISTINCE(*) WHERE \n{\n");
        for(Triple atom : atoms)
            sparql.append(atom + "\n");
        sparql.append("}\n");
        return Formulas.endpoint.count(sparql.toString());
    }

    //对atoms中出现的变量名进行重新整理，按照其出现的先后顺序
    public Formula strip(){
        Map<String, String> var_id = Maps.newHashMap();
        for(Triple tri : atoms){
            if(tri.h.startsWith(Formulas.prefix) && !var_id.containsKey(tri.h))
                    var_id.put(tri.h, Formulas.prefix + var_id.size());
            if(tri.r.startsWith(Formulas.prefix) && !var_id.containsKey(tri.r))
                    var_id.put(tri.r, Formulas.prefix + var_id.size());
            if(tri.t.startsWith(Formulas.prefix) && !var_id.containsKey(tri.t))
                    var_id.put(tri.t, Formulas.prefix + var_id.size());
        }
        vars.clear();
        for(int i = 0; i < var_id.size(); i ++)
            vars.add(Formulas.prefix + i);
        for(Triple tri : atoms){
            if(tri.h.startsWith(Formulas.prefix))
                tri.h = var_id.get(tri.h);
            if(tri.r.startsWith(Formulas.prefix))
                tri.r = var_id.get(tri.r);
            if(tri.t.startsWith(Formulas.prefix))
                tri.t = var_id.get(tri.t);
        }
        return this;
    }
}