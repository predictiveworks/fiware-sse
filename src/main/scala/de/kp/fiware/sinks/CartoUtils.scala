package de.kp.fiware.sinks
/*
 * Copyright (c) 2021 Dr. Krusche & Partner PartG. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 * 
 * @author Stefan Krusche, Dr. Krusche & Partner PartG
 * 
 */
import com.google.gson._

import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

object CartoUtils {
    /*
   * This method transforms attributes that describe a geo location
   * either as geo:json or as NGSI v2 Simple Location Format into a
   * PostGIS compliant representation.
   * 
   * All other attributes are converted to `null`
   */
  def attr2Geom(attrType:String, attrValu:JsonElement):String = {
    
    /*
     * The FIWARE Context Broker supports different geometric
     * representations. This method responds to them and converts
     * them in Carto compliant representations
     */
    val geom = attrType match {
      /** 
       *  GeoJSON 
       */
      case "geo:json" => {
        /*
         * The attribute value specifies a JSON object of the form
         * 
         *  "location": {
         *    "type": "geo:json",
         *    "value": {
         *       "coordinates": [1,1.1],
         *       "type": "Point"
         *    }
         *  }
         *  
         * IMPORTANT: geo:json uses LON-LAT while (all) other formats
         * leverage LAT-LON
         *  
         */
        val attrObj = attrValu.getAsJsonObject
        /* see PostGIS specification */
        s"ST_GeomFromGeoJSON('${attrObj.toString}')" // LON-LAT
      }
      /**
       * Simple Location Format of FIWARE Orion Context Broker
       */
      case "geo:box" => {               
        /*
         * Type `geo:box`: A bounding box is a rectangular region, 
         * often used to define the extents of a map or a rough area 
         * of interest. A box is represented by a two-length string 
         * array of latitude-longitude pairs. The first pair is the 
         * lower corner, the second is the upper corner.
         * 
         * {
         *  "location": {
         *    "value": [
         *      "40.63913831188419, -8.653321266174316",
         *      "40.63881265804603, -8.653149604797363"
         *    ],
         *    "type": "geo:box"
         *  }
         *}
         */
        val attrAry = attrValu.getAsJsonArray
        
        val lower = attrAry.get(0).getAsString.split(",")
        val upper = attrAry.get(1).getAsString.split(",")
        
        /*
         * 'x' represents the latitude and 'y' the longitude
         * 
         * ST_MakeEnvelope(xmin, ymin,xmax, ymax, srid)
         */
        s"ST_MakeEnvelope(${lower(0).trim}::double precision , ${lower(1).trim}::double precision , ${upper(0).trim}::double precision , ${upper(1).trim}::double precision , 4326)"        
      }
      case "geo:line" => {   
        /*
         * Type `geo:line`: the attribute value must contain a string 
         * array of valid latitude-longitude pairs. There must be at 
         * least two pairs.
         */
        val attrAry = attrValu.getAsJsonArray
        
        val points = ArrayBuffer.empty[String]
        attrAry.foreach(p => {
          
          val split = p.getAsString.split(",") 

          val lat = split(0).trim
          val lon = split(1).trim
        
          points += s"ST_MakePoint(${lat}::double precision , ${lon}::double precision ))" // LAT-LON
           
        })
        
        val line = s"ST_MakeLine(ARRAY[${points.mkString(" , ")}])"
        s"ST_SetSRID(${line}, 4326)"
      }
      case "geo:point" => {
        /*
         * Type `geo:point`: the attribute value must contain a string 
         * containing a valid latitude-longitude pair, separated by comma.
         * 
         * "location": {
         * 	 "type": "geo:point",
         *    "value": "19.435433, -99.133072",
         *    "metadata": {}
         *  },
         *  
         */
        val attrStr = attrValu.getAsString
        val attrSplit = attrStr.split(",")

        val lat = attrSplit(0).trim
        val lon = attrSplit(1).trim
      
        val point = s"ST_MakePoint(${lat}::double precision , ${lon}::double precision )" // LAT-LON
        s"ST_SetSRID(${point}, 4326)"
      }
      case "geo:polygon" => {        
        /*
         * Type `geo:polygon`: the attribute value must contain a string 
         * array of valid latitude-longitude pairs. There must be at least 
         * four pairs, with the last being identical to the first.
         * 
         * Coordinate pairs should be properly ordered so that the line
         * segments that compose the polygon remain on the outer edge of
         * the defined area.
         */
        val attrAry = attrValu.getAsJsonArray
        
        val points = ArrayBuffer.empty[String]
        attrAry.foreach(p => {
          
          val split = p.getAsString.split(",") 

          val lat = split(0).trim
          val lon = split(1).trim
        
          points += s"ST_MakePoint(${lat}::double precision , ${lon}::double precision ))" // LAT-LON
           
        })
        
        val line = s"ST_MakeLine(ARRAY[${points.mkString(" , ")}])"
        val polygon = s"ST_MakePolygon(${line})"

        s"ST_SetSRID(${polygon}, 4326)"
      }
      case _ => null
    }
    
    geom
    
  }

}