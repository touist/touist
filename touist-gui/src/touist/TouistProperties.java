package touist;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 * Note: to use that class, write:
 * 		public static TouistProperties properties;
 * @author maelv
 *
 */
public class TouistProperties {
	private static Map<String,String> properties = new HashMap<String,String>();
	
	/**
     * Loads the version.properties and fill the different
     * variables VERSION, BUILD_TIMESTAMP...
     */
    private Map<String, String> getPropertiesMap() {
    	if(properties != null) {
    		return properties;
    	}
		Properties prop = new Properties();
		String propFileName = "version.properties";
		InputStream inputStream = getClass().getClassLoader().getResourceAsStream(propFileName);
		try {
			if (inputStream != null) {
				prop.load(inputStream);
			} else {
				throw new FileNotFoundException("property file '" + propFileName + "' not found in the classpath");
			}
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		properties.put("minimum_java_version", prop.getProperty("minimum_java_version"));
		properties.put("version", prop.getProperty("version"));
		properties.put("timestamp", prop.getProperty("timestamp"));
		return properties;
    }
    
	public String getProperty(String property) {
		return getPropertiesMap().get(property);
	}
}
