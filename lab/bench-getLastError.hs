import Criterion.Main
import IOCP.Windows

main = defaultMain [bench "getLastError" getLastError]
