let
  region = "us-east-1";
  accessKeyId = "happy-hour";
in
{ 
  backend1 = { resources, ... }:
    { 
      deployment.targetEnv = "ec2";
      deployment.ec2.accessKeyId = accessKeyId;
      deployment.ec2.region = region;
      deployment.ec2.subnetId = "subnet-5bf9af77";
      deployment.ec2.instanceType = "t2.micro";
      deployment.ec2.keyPair = resources.ec2KeyPairs.my-free-tier;
    };
  resources.ec2KeyPairs.my-free-tier = { inherit region accessKeyId; };
}