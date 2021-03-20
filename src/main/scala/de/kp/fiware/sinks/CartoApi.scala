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
import java.net.URLEncoder

import akka.http.scaladsl._
import akka.http.scaladsl.model._

import com.google.gson._

import scala.concurrent.Await
import scala.concurrent.duration._

class CartoApi extends RestApi {
  
  private val endpoint = getEndpoint
  private val apiKey = cfg.getString("apiKey")
  
  private val duration = cfg.getInt("duration").seconds

  def create(table:String, columns:String):Unit = {
    
    try {

      val sql = s"CREATE TABLE ${table} (${columns})"
      
      /* Send to Carto */
      val response = executeSql(sql)
      
      val status = response.status
      if (status != StatusCodes.OK)
        throw new Exception(s"Query execution failed with: ${status.reason}")
            
      /* Make table visible */
      cartofyTable(table)
      
    } catch {
      case t:Throwable => throw new Exception(t.getMessage)
    }

  }
  
  def insert(table:String, columns:String, values:String):Unit = {
    
    try {

      val sql = s"INSERT INTO ${table} (${columns}) VALUES (${values})"
      
      /* Send to Carto */
      val response = executeSql(sql)
      
      val status = response.status
      if (status != StatusCodes.OK)
        throw new Exception(s"Query execution failed with: ${status.reason}")
      
    } catch {
      case t:Throwable => throw new Exception(t.getMessage)
    }
    
  }
  
  def update(table:String, sets:String, where:String):Unit = {
    
    try {

      val sql = s"UPDATE ${table} SET ${sets} WHERE ${where}"
      
      /* Send to Carto */
      val response = executeSql(sql)
      
      val status = response.status
      if (status != StatusCodes.OK)
        throw new Exception(s"Query execution failed with: ${status.reason}")
      
    } catch {
      case t:Throwable => throw new Exception(t.getMessage)
    }
    
  }

  def exists(table:String):Boolean = {
    
    try {

      val sql = s"SELECT COUNT(*) FROM ${table}"
      
      /* Send to Carto */
      val response = executeSql(sql)
      
      val status = response.status
      if (status != StatusCodes.OK)
        throw new Exception(s"Query execution failed with: ${status.reason}")
      /*
       * {
  			 *  "time": 0.007,
  		   *  "total_rows": 1,
       *  "rows": [
       *    {
       *      "count": 4994
       *    }
       *  ]
       * }
       */
      val payload = getPayload(response)
      val rows = payload.get("rows").getAsJsonArray
      
      val head = rows.get(0).getAsJsonObject
      val count = head.get("count").getAsLong

      count != 0L

    } catch {
      case t:Throwable => false
    }

  }
  /*
   * A helper method to make a certain table visible
   * for the Carto dashboard
   */
  private def cartofyTable(table:String):Unit = {

    val sql = s"SELECT cdb_cartodbfytable(${table})"
    
    /* Send to Carto */
    val response = executeSql(sql)
    
    val status = response.status
    if (status != StatusCodes.OK)
      throw new Exception(s"Query execution failed with: ${status.reason}")
    
  }
  
  private def executeSql(sql:String):HttpResponse = {

    val query = URLEncoder.encode(sql, "UTF-8")

    /* Build request */
    val url = s"${endpoint}/api/v2/sql?q=${query}&api_key=${apiKey}"
    val request = HttpRequest(HttpMethods.GET, url)
    
    /* Send to Carto */
    val future = doRequest(request)
    val response = Await.result(future, duration)
    
    response
    
  }
}