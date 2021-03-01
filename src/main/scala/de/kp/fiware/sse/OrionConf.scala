package de.kp.fiware.sse
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
import com.typesafe.config.{Config,ConfigFactory}

import scala.collection.JavaConversions._

case class HttpBinding(host:String, port:Int)

object OrionConf {

  private val path = "reference.conf"
  private var systemName = "fiware-sse"  
  /*
   * Subscription support
   */
  private var subscription = ""
  private var subscriptionData = ""
  
  private var brokerUrl = ""

  private var httpHost = "127.0.0.1"
  private var httpPort = 9060

  /*
   * This is the reference to the overall configuration
   * file that holds all configuration required for this
   * application
   */
  private var cfg:Config = null
  
  def init(config:Option[String] = None):Boolean = {
    
    try {

      cfg = if (config.isDefined) {
        /*
         * An external configuration file is provided
         * and must be transformed into a Config
         */
        ConfigFactory.parseString(config.get)
        
      } else {
        /*
         * The internal reference file is used to
         * extract the required configurations
         */
        ConfigFactory.load(path)
        
      }

      extractCfg
      true
      
    } catch {
      case t:Throwable => {
        t.printStackTrace()
        false
      }
    }
  }

  private def extractCfg:Unit = {

    /* HTTP SERVER */

    val binding = cfg.getConfig("binding")
    
    httpHost = binding.getString("host")
    httpPort = binding.getInt("port")
    
    /* BROKER */
    
    val broker = cfg.getConfig("broker")
    brokerUrl = broker.getString("endpoint")

    /* SUBSCRIPTION DATA */
    
    subscriptionData = buildSubscriptionData
    println(subscriptionData)
    
  }
   
  def getBrokerUrl = brokerUrl
  /*
   * Retrieve the SSL/TLS configuration for subscription
   * requests to the Orion Context Broker
   */
  def getClientSecurity:Config = {

    val security = cfg.getConfig("security")
    security.getConfig("client")
  
  }
  /*
   * The host & port configuration of the HTTP server that
   * is used as a notification endpoint for an Orion Context
   * Broker instance
   */
  def getHttpBinding = HttpBinding(httpHost, httpPort)
  /*
   * Retrieve the SSL/TLS configuration for notification
   * requests from the Orion Context Broker
   */
  def getServerSecurity:Config = {

    val security = cfg.getConfig("security")
    security.getConfig("server")
  
  }  
  /*
   * The subscription identifier used to register and uniquely
   * distinguish notifications from all other notications
   */
  def getSubscription = subscription
  /*
   * Retrieve the body of the Orion Context Broker subscription
   */
  def getSubscriptionData = subscriptionData
  /*
   * The name of Actor System used
   */
  def getSystemName = systemName
  /*
   * This method is used to register the subscription id
   * retrieved from the Orion Context Broker
   */
  def setSubscription(sid:String):Unit = {
    subscription = sid
  }
  
  private def buildSubscriptionData:String = {
    
    val cSubscription = cfg.getConfig("subscription")

    val jSubscription = new JsonObject()
    
    /* 'description' */
    
    jSubscription.addProperty("description", cSubscription.getString("description"))
    
    /* subject */
    
    val cSubject = cSubscription.getConfig("subject")
    val jSubject = new JsonObject()
    
    /* subject :: entities */
    
    val cEntities = cSubject.getConfigList("entities")
    val jEntities = new JsonArray()
    
    cEntities.foreach(cEntity => {
      
      val jEntity = new JsonObject()
      
      /* entity :: id */
      
      val eid = cEntity.getString("id")
      jEntity.addProperty("id", eid)
      
      /* entity :: type */
      
      val etype = cEntity.getString("type")
      jEntity.addProperty("type", etype)
      
      jEntities.add(jEntity)
      
    })
    
    jSubject.add("entities", jEntities)
    
    
    /* subject :: condition */
    
    val cCondition = cSubject.getConfig("condition")
    val jCondition = new JsonObject()
    
    val cConditionAttrs = cCondition.getStringList("attrs")
    val jConditionAttrs = new JsonArray()
    
    cConditionAttrs.foreach(attr => jConditionAttrs.add(attr))
    jCondition.add("attrs", jConditionAttrs)
    
    jSubject.add("condition", jCondition)
    jSubscription.add("subject", jSubject)
    
    /* notification */
    
    val cNotification = cSubscription.getConfig("notification")
    val jNotification = new JsonObject()
    
    /* notification :: http */
    
    val cHttp = cNotification.getConfig("http")
    val jHttp = new JsonObject()
    
    jHttp.addProperty("url", cHttp.getString("url"))
    jNotification.add("http", jHttp)
    
    /* notification :: attrs */
    
    val cNotificationAttrs = cNotification.getStringList("attrs")
    val jNotificationAttrs = new JsonArray()
    
    cNotificationAttrs.foreach(attr => jNotificationAttrs.add(attr))
    jNotification.add("attrs", jNotificationAttrs)
    
    jSubscription.add("notification", jNotification)
    
    /* expires */
    
    jSubscription.addProperty("expires", cSubscription.getString("expires"))
    
    /* throttling */
    
    jSubscription.addProperty("throttling", cSubscription.getInt("throttling"))
    jSubscription.toString

  }
}