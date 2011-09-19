import sisc.modules.io.Buffer;
import base64.Base64;
import java.io.IOException;

/*public class Base64Bridge {
    public static String encodeSchemeObject(sisc.data.Closure x) throws java.io.IOException {
	return Base64.encodeObject((java.io.Serializable)x, Base64.URL_SAFE);
    }

    public static String encodeSchemeObject(Object x) throws java.io.IOException {
	return Base64.encodeObject((java.io.Serializable)x, Base64.URL_SAFE);
    }
    }*/

public class Base64Bridge {
    public static String encodeSchemeObject(byte[] x) throws java.io.IOException {
	return Base64.encodeBytes(x, Base64.URL_SAFE);
    }
}


