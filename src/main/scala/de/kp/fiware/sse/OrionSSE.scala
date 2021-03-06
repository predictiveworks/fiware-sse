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

import scopt.OptionParser

object OrionSSE {

  private case class CliConfig(
    /* 
     * The command line interface supports the provisioning 
     * of a typesafe config compliant configuration file
     */
    conf:String = null
  )
  
  def main(args:Array[String]):Unit = {
    
    /* Command line argument parser */
    val parser = new OptionParser[CliConfig]("Orion SSE") {
      
      head("Orion BE: NGSI-v2 Orion Consumer and SSE Producer .")
      
      opt[String]("c")
        .text("The path to the configuration file.")
        .action((x, c) => c.copy(conf = x))
    
    }

    /* Parse the argument and then run */
    parser.parse(args, CliConfig()).map{c =>
      
      try {

        if (c.conf == null) {
          
          println("[INFO] ------------------------------------------------")
          println("[INFO] Launch Orion SSE with internal configuration.")
          println("[INFO] ------------------------------------------------")
          
          new OrionLauncher().launch()
        
        } else {
          
          println("[INFO] ------------------------------------------------")
          println("[INFO] Launch Orion SSE with external configuration.")
          println("[INFO] ------------------------------------------------")
          
          val config = scala.io.Source.fromFile(c.conf).getLines.mkString("\n")
          new OrionLauncher().launch(Option(config))
            
        }
          
        println("[INFO] ------------------------------------------------")
        println("[INFO] Orion SSE service started.")
        println("[INFO] ------------------------------------------------")

      } catch {
        case t:Throwable => {
          
          println("[ERROR] ------------------------------------------------")
          println("[ERROR] Orion SSE cannot be started: " + t.getMessage)
          println("[ERROR] ------------------------------------------------")
          
        }
      }
    }.getOrElse {
      /* 
       * Sleep for 10 seconds so that one may see error messages 
       * in Yarn clusters where logs are not stored. 
       */     
      Thread.sleep(10000)
      sys.exit(1)
    }

  }

}