import qualified Data.Text as T

formatSSH :: (String, String) -> String
formatSSH (nid, ip) = "Host quorum-cluster-" ++ nid ++ "\n\
                    \     Hostname " ++ (removeComma ip) ++ "\n\
                    \     User ubuntu\n\
                    \     IdentitiesOnly yes\n\
                    \     Port 22\n\
                    \     IdentityFile /Users/zekunshi/workspace/quorum-aws/terraform/secrets/ec2-keys/quorum-demo.pem\n\
                    \"
  where removeComma xs = [ x | x <- xs, not (x `elem` ",") ]

formatSSHs :: [String] -> String
formatSSHs ips = concat $ fmap formatSSH $ zip (map show [1..length ips]) ips

main :: IO ()
main = do
      s <- readFile sshconfig
      seq (length s) (return ())
      ips <- readFile raw
      seq (length ips) (return ())
      let sshs = formatSSHs $ Prelude.lines ips
      let vagrant = Prelude.head $ T.splitOn (T.pack "\n\n") (T.pack s)
      let final = T.unpack vagrant ++ "\n\n" ++ sshs
      putStr final
      seq final (writeFile sshconfig final)
    where sshconfig = "/Users/zekunshi/.ssh/config"
          raw = "ssh.dat"
