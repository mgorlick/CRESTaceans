/*
   Copyright 2009 Michael M. Gorlick

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
         http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

// A wrapper around the JSON simple parser for parsing JSON text into Scheme lists and hash tables.

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

public class jsonParserTranslator {
    JSONParser parser;

    public jsonParserTranslator(int i) { // Why do I have to have a junk argument to make java-new work?
	parser = new JSONParser();
    }

    public Object parse(String s) throws ParseException {
	return parser.parse(s);
    }

    // Used to disinguish among the various JSON "types" at the Scheme level.
    // DO NOT change the numbering without also changing :translation-vector: in json.scm.
    public static int flavor(Boolean x) {
	return 0;
    }

    public static int flavor(Long x) {
	return 1;
    }

    public static int flavor(Float x) {
	return 1;
    }

    public static int flavor(Double x) {
	return 1;
    }

    public static int flavor(String x) {
	return 2;
    }

    // List.
    public static int flavor(JSONArray x) {
	return 3;
    }

    // Hash table.
    public static int flavor(JSONObject x) {
	return 4;
    }

    // Give a JSONObject return an interator for its collection of name/value pairs.
    public static Iterator mapIterator(JSONObject x) {
	return x.entrySet().iterator();
    }

    // Given an interator return each value. If iterator is exhausted return null.
    public static Map.Entry next(Iterator i) {
	if (i.hasNext())
	    return (Map.Entry)i.next();
	else
	    return null;
    }
}

