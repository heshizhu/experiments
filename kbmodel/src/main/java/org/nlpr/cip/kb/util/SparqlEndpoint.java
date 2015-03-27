package org.nlpr.cip.kb.util;

import com.google.common.base.Joiner;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.apache.log4j.Logger;

import java.io.*;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.util.List;
import java.util.Map;

/**
 * User: hesz
 * Date: 2014-12-9
 * Time: 11:58
 */
public class SparqlEndpoint {
    private static Logger log = Logger.getLogger(SparqlEndpoint.class);

    public static String queryFormat = "text/tab-separated-values";//以\t为分割符
    public static String defaultHost = "172.18.28.117";//主机地址:172.18.28.117
    public static int defaultPort = 3039;

    private String host;//主机地址:172.18.28.117
    private int port;//端口号:3039

    private String endpoint;//http://172.18.28.117:3039/sparql

    private static Map<String, SparqlEndpoint> sparqlEndpointPool = Maps.newHashMap();
    public static SparqlEndpoint getSparqlEndpoint(){
        return getSparqlEndpoint(defaultHost, defaultPort);
    }
    public static SparqlEndpoint getSparqlEndpoint(String host, int port){
        String endpoint = String.format("http://%s:%d/sparql", host, port);
        SparqlEndpoint sparqlEndpoint = sparqlEndpointPool.get(endpoint);
        if(sparqlEndpoint == null){
            sparqlEndpointPool.put(endpoint, sparqlEndpoint = new SparqlEndpoint(host, port));
            log.info("start sparql endpoint at:\t" + endpoint);
        }
        return sparqlEndpoint;
    }

    private SparqlEndpoint(String host, int port) {
        this.host = host;
        this.port = port;
        this.endpoint = String.format("http://%s:%d/sparql", host, port);
    }


    public boolean ask(String query) {
        String queryResult = makeQuery(query);
        String[] terms = queryResult.split("\n");
        if (terms.length == 2) {
            if(terms[1].equalsIgnoreCase("1"))
                return true;
        }
        return false;
    }

    public long count(String query) {
        String queryResult = makeQuery(query);
        String[] terms = queryResult.split("\n");
        if (terms.length == 2) {
            try {
                return Long.parseLong(terms[1]);
            }
            catch(NumberFormatException ex){
                return Long.MAX_VALUE;
            }
        }
        return -1;
    }


    public List<String> query(String query) {
        List<String> results = Lists.newArrayList();
        String queryResult = makeQuery(query);
        for (String line : queryResult.split("\n")) {
            results.add(line);
        }
        return results;
    }

    public String queryEntry(String query) {
        String queryResult = makeQuery(query).split("\n")[1];
        if(queryResult.startsWith("\"") && queryResult.endsWith("\""))
            queryResult = queryResult.substring(1, queryResult.length() - 1);
        return queryResult;
    }

    public String getQueryURI(String query) {
        try {
            String fullQuery = String.format(
                    "%s\n%s", Utils.getQueryPrefix(), query);
            String encodeQuery = URLEncoder.encode(fullQuery, "utf-8");
            String finalQuery = String.format("%s?query=%s&format=%s", endpoint, encodeQuery, queryFormat);
            return finalQuery;

        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
            System.out.println("error occur in encode url: " + query);
            return "";
        }
    }

    //获得查询Reader
    public BufferedReader queryReader(String query){
        try {
            String fullQuery = String.format(
                    "%s\n%s", Utils.getQueryPrefix(), query);
            String encodeQuery = URLEncoder.encode(fullQuery, "utf-8");
            String finalQuery = String.format("%s?query=%s&format=%s", endpoint, encodeQuery, queryFormat);
            return getURLContentReader(finalQuery.toString());

        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
            System.out.println("error occur in encode url: " + query);
            return null;
        }
    }

    public String makeQuery(String query) {
        try {
            String fullQuery = String.format(
                    "%s\n%s", Utils.getQueryPrefix(), query);
            String encodeQuery = URLEncoder.encode(fullQuery, "utf-8");
            String finalQuery = String.format("%s?query=%s&format=%s", endpoint, encodeQuery, queryFormat);
            return getURLContent(finalQuery.toString());

        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
            System.out.println("error occur in encode url: " + query);
            return "";
        }
    }

    //得到属性的类别，有owl:ObjectProperty、owl:DatatypeProperty
    public String getPropertyType(String property){
        String sparql = String.format(
                "SELECT ?x\n" +
                        "WHERE{\n" +
                        "%s <http://rdf.freebase.com/ns/type.object.type> ?x .\n" +
                        "}", property);
        return queryEntry(sparql);
    }

    //得到属性的定义域和值域类型
    public String[] getPropertyDomainRange(String property){
        String[] results = new String[2];
        results[0] = getPropertyDomain(property);
        results[1] = getPropertyRange(property);
        return results;
    }

    public String getPropertyDomain(String property){
        String sparql = String.format(
                "SELECT ?x\n" +
                        "WHERE{\n" +
                        "%s <http://rdf.freebase.com/ns/type.property.schema> ?x .\n" +
                        "}", property);
        return queryEntry(sparql);
    }

    public String getPropertyRange(String property){
        String sparql = String.format(
                "SELECT ?x\n" +
                        "WHERE{\n" +
                        "%s <http://rdf.freebase.com/ns/type.property.expected_type> ?x .\n" +
                        "}", property);
        return queryEntry(sparql);
    }

    public static InputStream getURIInputStream(String uri) throws IOException {
        URLConnection conn = new URL(uri).openConnection();
        return conn.getInputStream();
    }

    public static BufferedReader getURLContentReader(String strURL) {
        try {
            URLConnection conn = new URL(strURL).openConnection();
            BufferedReader reader = new BufferedReader(
                    new InputStreamReader(conn.getInputStream(), "utf-8"));
            return reader;

        } catch (IOException e) {
            System.out.println("error occur in get content from: " + strURL);
        }
        return null;
    }

    public static String getURLContent(String strURL) {
        StringBuilder contents = new StringBuilder();
        try {
            URLConnection conn = new URL(strURL).openConnection();
            BufferedReader reader = new BufferedReader(
                    new InputStreamReader(conn.getInputStream(), "utf-8"));
            while (true) {
                String line = reader.readLine();
                if (line == null)
                    break;
                contents.append(line + "\n");
            }
            reader.close();
        } catch (IOException e) {
            System.out.println("error occur in get content from: " + strURL);
        }
        return contents.toString().trim();
    }

    public static void main(String[] args) throws IOException {
        SparqlEndpoint endpoint = new SparqlEndpoint("192.168.238.149", 4051);

        String property = "fb:people.person.spouse_s";
        System.out.println(endpoint.getPropertyType(property));
        System.out.println(endpoint.getPropertyDomain(property));
        System.out.println(endpoint.getPropertyRange(property));


        String sparql = String.format(
                        "SELECT count(distinct ?x) AS ?count{\n" +
                        "?x %s ?o .\n" +
                        "} ", property);
        System.out.println(endpoint.queryEntry(sparql));
        System.out.println(endpoint.count(sparql));

        sparql = String.format(
                "SELECT * WHERE {\n" +
                "fb:en.yao_ming ?p ?o .\n" +
                "}");
        List<String> contents = endpoint.query(sparql);
        System.out.println(Joiner.on("\n").join(contents));

        sparql = "ASK WHERE {\n" +
                "fb:en.yao_ming fb:people.person.spouse_s fb:m.0j688yq .\n" +
                "}\n";
        System.out.println(endpoint.ask(sparql));


    }
}
