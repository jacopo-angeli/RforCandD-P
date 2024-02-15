with Ada.Text_IO; use Ada.Text_IO;
with Node;

procedure Main is
   -- Declare variables to hold references to the nodes
   Node1 : Node.NodeAccess;
   Node2 : Node.NodeAccess;
   Node3 : Node.NodeAccess;
begin
   -- Create instances of the Node task
   Node1 := new Node.Node (1, Node.NodeAccessArray'Access);
   Node2 := new Node.Node (2, Node.NodeAccessArray'Access);
   Node3 := new Node.Node (3, Node.NodeAccessArray'Access);

   -- Store references to the nodes in the array
   Node.NodeAccessArray (1) := Node1;
   Node.NodeAccessArray (2) := Node2;
   Node.NodeAccessArray (3) := Node3;

   Put_Line ("Nodes created successfully.");
end Main;
