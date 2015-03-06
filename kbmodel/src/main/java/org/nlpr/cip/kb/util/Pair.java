package org.nlpr.cip.kb.util;


import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.io.Serializable;

/**
 * User: hesz
 * Date: 14-3-13
 * Time: 上午11:26
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class Pair<F, S> implements Serializable {

    @JsonProperty("first") public F first;
    @JsonProperty("second") public S second;

    @JsonCreator
    public Pair(@JsonProperty("first") F first, @JsonProperty("second") S second)
    {
        this.first = first;
        this.second = second;
    }

    public static <T, S> Pair<T, S> createNewPair(T left, S right)
    {
        return new Pair<T, S>(left, right);
    }

    public F getFirst() {
        return first;
    }

    public S getSecond() {
        return second;
    }

    public void setFirst(F pFirst) {
        first = pFirst;
    }

    public void setSecond(S pSecond) {
        second = pSecond;
    }

    public Pair<S, F> reverse() {
        return new Pair<S, F>(second, first);
    }

    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof Pair))
            return false;

        final Pair pair = (Pair) o;

        if (first != null ? !first.equals(pair.first) : pair.first != null)
            return false;
        if (second != null ? !second.equals(pair.second) : pair.second != null)
            return false;

        return true;
    }

    public int hashCode() {
        int result;
        result = (first != null ? first.hashCode() : 0);
        result = 29 * result + (second != null ? second.hashCode() : 0);
        return result;
    }

    public String toString() {
        return "(" + getFirst() + ", " + getSecond() + ")";
    }
}