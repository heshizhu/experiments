package org.nlpr.cip.kb.logic;

import java.util.Map;

/**
 * User: hesz
 * Date: 2015-3-20
 * Time: 12:23
 */
public class Triple {
    public String h, r, t;
    public Triple(String t1, String t2, String t3){ h = t1; r = t2; t = t3;}
    public Triple(Triple that){ h = that.h; r = that.r; t = that.t; }

    public Triple replaceVar(Map<String, String> matcher){
        Triple new_one = new Triple(this);
        if(matcher.containsKey(h))
            new_one.h = matcher.get(h);
        if(matcher.containsKey(r))
            new_one.r = matcher.get(r);
        if(matcher.containsKey(t))
            new_one.t = matcher.get(t);
        return new_one;
    }

    @Override
    public boolean equals(Object obj) {
        if(!(obj instanceof Triple)) return false;
        Triple that = (Triple)obj;
        if(!h.equals(that.h)) return false;
        if(!r.equals(that.r)) return false;
        if(!t.equals(that.t)) return false;
        return true;
    }

    @Override
    public String toString() {
        return String.format("%s\t%s\t%s .", h, r, t);
    }
}
