package hypermake

import zio.Has

package object cli {

  type CLI = Has[CLI.Service]

}
