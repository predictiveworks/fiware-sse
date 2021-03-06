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
import akka.http.scaladsl.{ConnectionContext, Http}
import com.typesafe.config.Config

import javax.net.ssl.SSLContext

object SslHelper {
  
  def buildCartoContext = {
    val cfg = OrionConf.getCartoSecurity    
    ConnectionContext.httpsClient(context = buildTrustAllContext(cfg))
  }
  
  def buildFiwareContext = {
    val cfg = OrionConf.getFiwareSecurity    
    ConnectionContext.httpsClient(context = buildSSLContext(cfg))
  }
  
  def buildServerContext = {
    val cfg = OrionConf.getServerSecurity    
    ConnectionContext.httpsServer(sslContext = buildSSLContext(cfg))
  }

  private def buildTrustAllContext(securityCfg:Config):SSLContext = {
    
    val sslOptions = getSslOptions(securityCfg)
    sslOptions.getTrustAllContext
    
  }
  
  private def buildSSLContext(securityCfg:Config):SSLContext = {
    
    val sslOptions = getSslOptions(securityCfg)
    sslOptions.getSSLContext
    
  }
  
  private def getSslOptions(securityCfg:Config): SslOptions = {

    val ksFile = {
      val v = securityCfg.getString("ksFile")
      if (v.isEmpty) None else Option(v)
    }

    val ksType = {
      val v = securityCfg.getString("ksType")
      if (v.isEmpty) None else Option(v)
    }

    val ksPass = {
      val v = securityCfg.getString("ksPass")
      if (v.isEmpty) None else Option(v)
    }

    val ksAlgo = {
      val v = securityCfg.getString("ksAlgo")
      if (v.isEmpty) None else Option(v)
    }

    val tsFile = {
      val v = securityCfg.getString("tsFile")
      if (v.isEmpty) None else Option(v)
    }

    val tsType = {
      val v = securityCfg.getString("tsType")
      if (v.isEmpty) None else Option(v)
    }

    val tsPass = {
      val v = securityCfg.getString("tsPass")
      if (v.isEmpty) None else Option(v)
    }

    val tsAlgo = {
      val v = securityCfg.getString("tsAlgo")
      if (v.isEmpty) None else Option(v)
    }

    val caCertFile = {
      val v = securityCfg.getString("caCertFile")
      if (v.isEmpty) None else Option(v)
    }

    val certFile = {
      val v = securityCfg.getString("certFile")
      if (v.isEmpty) None else Option(v)
    }

    val privateKeyFile = {
      val v = securityCfg.getString("privateKeyFile")
      if (v.isEmpty) None else Option(v)
    }

    val privateKeyFilePass = {
      val v = securityCfg.getString("privateKeyFilePass")
      if (v.isEmpty) None else Option(v)
    }
    
    new SslOptions(ksFile, ksType, ksPass, ksAlgo, tsFile, tsType, tsPass, tsAlgo, caCertFile, certFile, privateKeyFile, privateKeyFilePass)
    
  }

}