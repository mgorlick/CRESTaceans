/*
   Copyright 2009 Alegria Baquero, Michael M. Gorlick

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

/*
  A helper class for the Scheme interface to the Abdera Atom feed parser.
   The following jars are required from the Abdera distribution:

   abdera-0.4.0-incubating.jar
   abdera-i18n-0.4.0-incubating.jar
   abdera-parser-0.4.0-incubating.jar
   axiom-api-1.2.5.jar
   axiom-impl-1.2.5.jar
   commons-logging-1.0.4.jar
   geronimo-activation_1.0.2_spec-1.1.jar
   geronimo-stax-api_1.0_spec-1.0.1.jar
   jaxen-1.1.1.jar
   wstx-asl-3.2.1.jar

   In addition it requires:

   Java Activation Framework (JAF 1.1.1) activation.jar
   (http://java.sun.com/javase/technologies/desktop/javabeans/jaf/index.jsp)

   com.springsource.javax.xml.stream-1.0.1.jar
*/

import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;
import java.util.List;

import org.apache.abdera.Abdera;
import org.apache.abdera.model.Document;
import org.apache.abdera.model.Feed;
import org.apache.abdera.parser.Parser;
import org.apache.abdera.model.Entry;

public class AbderaParser {
    private Feed feed;
    private List<Entry> entryList;

    // Obtain a list of Atom Entries from an Atom feed.
    public AbderaParser(String atomFeed) throws UnsupportedEncodingException {
	Parser parser = Abdera.getInstance().getParser(); // Fetch the static Parser instance.
	byte[] bytes;

	bytes = atomFeed.getBytes("UTF-8");
	Document<Feed> feed_doc = parser.parse(new ByteArrayInputStream(bytes));
	feed = feed_doc.getRoot();
	entryList = feed.getEntries();
    }

    public List<Entry> getEntryList() {
	return entryList;
    }
}
