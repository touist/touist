package touist;

import java.io.IOException;
import java.util.Properties;

/**
 * Note: to use that class, write:
 * 		private static TouistProperties properties = new TouistProperties();
 * @author maelv
 *
 */
public class TouistProperties {
	private static Properties prop = new Properties();
	public TouistProperties() {
		try {
			prop.load(this.getClass().getClassLoader().getResourceAsStream("version.properties"));
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
    
	public String getProperty(String property) {
		return prop.getProperty(property);
	}
}
