package touist;

import java.io.IOException;
import java.io.InputStream;
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
			InputStream in = this.getClass().getClassLoader().getResourceAsStream("version.properties");
			if(in == null) throw new IOException();
			else prop.load(in);
		} catch (IOException e) {
			System.out.println("Warning: the property file version.properties was not found");
		}
	}
    
	public String getProperty(String property) {
		String res = prop.getProperty(property);
		return (res != null)?res:"";
	}
}
