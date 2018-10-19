let
  region = "us-east-1";
  accessKeyId = "hhkey";
in
{ 
  backend1 = { resources, ... }:
    { 
      deployment.targetEnv = "ec2";
      deployment.ec2.accessKeyId = accessKeyId;
      deployment.ec2.region = region;
      deployment.ec2.subnetId = "subnet-5bf9af77";
      deployment.ec2.instanceType = "t2.micro";
      deployment.ec2.associatePublicIpAddress = true;
      deployment.ec2.keyPair = "my-free-tier";
      deployment.ec2.privateKey = "~/.ssh/my-free-tier.pem";
    };
}