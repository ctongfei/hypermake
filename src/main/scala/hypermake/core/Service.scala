package hypermake.core

import hypermake.collection._


class Service(val name: Name,
              val env: Name,
              val `case`: Case,
              val inputs: Map[Name, Value],
              val inputEnvs: Map[Name, Name],
              val rawScript: Script
             ) {

}
