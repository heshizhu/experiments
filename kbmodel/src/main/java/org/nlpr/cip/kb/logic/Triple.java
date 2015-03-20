package org.nlpr.cip.kb.logic;

/**
 * User: hesz
 * Date: 2015-3-20
 * Time: 12:23
 */
public class Triple {
    public String h, r, t;
    public Triple(String t1, String t2, String t3){ h = t1; r = t2; t = t3;}
    public Triple(Triple that){ h = that.h; r = that.r; t = that.t; }

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
