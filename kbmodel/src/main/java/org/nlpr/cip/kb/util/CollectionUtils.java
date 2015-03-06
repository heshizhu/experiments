package org.nlpr.cip.kb.util;

import org.apache.log4j.Logger;

import java.util.*;

public class CollectionUtils {
	private static Logger log = Logger.getLogger(CollectionUtils.class);

	// 按照key的String值进行顺序排序
	public static List sortMapByKeyASC(Map map) {
		List list = new ArrayList(map.entrySet());

		Collections.sort(list, new Comparator() {
            public int compare(Object o1, Object o2) {
                return ((Comparable) ((Map.Entry) (o1)).getKey())
                        .compareTo(((Map.Entry) (o2)).getKey());
            }
        });

		return list;
	}

	// 按照key进行顺序排序
	public static List sortMapByKeyDESC(Map map) {
		List list = new ArrayList(map.entrySet());

		Collections.sort(list, new Comparator() {
            public int compare(Object o1, Object o2) {
                return ((Comparable) ((Map.Entry) (o2)).getKey())
                        .compareTo(((Map.Entry) (o1)).getKey());
            }
        });

		return list;
	}

	// 按照key进行顺序排序
	public static List sortMapByValueASC(Map map) {
		List list = new ArrayList(map.entrySet());

		Collections.sort(list, new Comparator() {
            public int compare(Object o1, Object o2) {
                return ((Comparable) ((Map.Entry) (o1)).getValue())
                        .compareTo(((Map.Entry) (o2)).getValue());
            }
        });

		return list;
	}

	// 按照key进行顺序排序
	public static List sortMapByValueDESC(Map map) {
		List list = new ArrayList(map.entrySet());

		Collections.sort(list, new Comparator() {
            public int compare(Object o1, Object o2) {
                return ((Comparable) ((Map.Entry) (o2)).getValue())
                        .compareTo(((Map.Entry) (o1)).getValue());
            }
        });

		return list;
	}
	
	public static void main(String[] args) {
//		Map<String, String> unsortMap = new HashMap<String, String>();
//		unsortMap.put("2", "B");
//		unsortMap.put("1", "A");
//		unsortMap.put("4", "D");
//		unsortMap.put("3", "B");
//		unsortMap.put("7", "C");
//		unsortMap.put("5", "z");
//		unsortMap.put("6", "b");
//		unsortMap.put("8", "a");
//		
//		for(Object obj : sortMapByKeyDESC(unsortMap))
//		{
//			Map.Entry entry = (Map.Entry)obj;
//			System.out.println(entry.getKey() + "-->" + entry.getValue());
//		}
		
		
		

		
		List<Double> ll = new ArrayList<Double>();
		ll.add(123.3);
		ll.add(2.2);
		
		Collections.sort(ll, new Comparator<Double>() {
            public int compare(Double d2, Double d1) {
                if ((d1 - d2) > 0)
                    return 1;
                else if ((d1 - d2) < 0)
                    return -1;
                else
                    return 0;
            }
        });
		for(Double l : ll)
		{
			System.out.println(l);
		}
	}
}
