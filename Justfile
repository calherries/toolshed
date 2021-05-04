deploy:
  clj -A:release tag
  # remember to update the version in the pom.xml
  clj -A:release jar 
  clj -A:release deploy