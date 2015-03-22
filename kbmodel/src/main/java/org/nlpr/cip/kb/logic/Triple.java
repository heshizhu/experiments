package org.nlpr.cip.kb.logic;

import com.google.common.base.Joiner;

import java.util.Map;

/**
 * User: hesz
 * Date: 2015-3-20
 * Time: 12:23
 */
public class Triple {
    public String[] terms = new String[3];

    public Triple(String[] terms){
        this.terms = terms;
    }

    public Triple(String t1, String t2, String t3){
        terms[0] = t1; terms[1] = t2; terms[2] = t3;
    }

    public Triple(Triple that){
        for(int i = 0; i < 3; i ++) terms[i] = that.terms[i];
    }

    public Triple replaceVar(Map<String, String> matcher){
        Triple new_one = new Triple(this);
        for(int i = 0; i < 3; i ++)
            if(matcher.containsKey(new_one.terms[i]))
                new_one.terms[i] = matcher.get(new_one.terms[i]);
        return new_one;
    }

    @Override
    public boolean equals(Object obj) {
        if(!(obj instanceof Triple)) return false;
        Triple that = (Triple)obj;
        for(int i = 0; i < 3; i ++)
            if(!terms[i].equals(that.terms[i])) return false;
        return true;
    }

    @Override
    public String toString() {
        return Joiner.on("\t").join(terms) + " .";
    }
}
