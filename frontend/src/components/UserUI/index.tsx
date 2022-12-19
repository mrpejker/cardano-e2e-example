import React, { useEffect, useState } from "react"
import { Container, Navbar, Nav, Button, Modal, Form, Table } from "react-bootstrap";
import Row from 'react-bootstrap/Row';
import Col from 'react-bootstrap/Col';
import { UserEndpoints, ObsState, UtxoInfo } from "src/contractEndpoints/user";
import { mkStartParams } from "src/contractEndpoints/parameters";

// Main component for the UserUI. It includes all the other components.
function UserUI() {
  const [currentContractState, setCurrentContractState] = useState<ObsState>([])
  const [contractEndpoints, setContractEndpoints] = useState<UserEndpoints>(new UserEndpoints([]));
  const [isConnected, setIsConnected] = useState(false);
  const [showStartModal, setShowStartModal] = useState(false);

  const handleShow = () => setShowStartModal(true);
  return (
    <Container>
      <Navbar sticky="top" bg="light">
        <Navbar.Brand><h1>Exchange Escrow</h1></Navbar.Brand>
        <Connect
          setCurrentContractState={setCurrentContractState}
          contractEndpoints={contractEndpoints}
          setIsConnected={setIsConnected}
        />
      </Navbar>
      <br></br>
      <Button
          style={{ marginRight: "20px" }}
          variant="primary"
          size="lg"
          onClick={handleShow}
          disabled={!isConnected}
      >
        Start new Escrow
      </Button>
      <Start
        show={showStartModal}
        handleShow={handleShow}
        setShowStartModal={setShowStartModal}
        contractEndpoints={contractEndpoints}
        />

      <Button
          style={{ marginRight: "20px" }}
          variant="primary"
          size="lg"
          disabled={!isConnected}
          onClick={async () =>
            console.log("Cancel")
          }
      >
        Cancel an Escrow
      </Button>
      <br></br>
      <br></br>
      <h2> Active Escrows </h2>
      <br></br>
      <ContractInformation
        currentContractState={currentContractState}
      />

    </Container>
  )
}

// Connect component that handles the wallet and endpoint connection.
const Connect = ({ setCurrentContractState, contractEndpoints, setIsConnected }) => {
  return (
    <Navbar.Collapse className="justify-content-end">
      <Nav.Link className="d-flex">
        <select
          style={{ marginRight: "16px" }}
          id="comboA"
          onChange={e => {
            console.log("Value Changed")
            console.log(e.target.value)
          }}
          defaultValue={"nami"}
        >
          <option value="nami">Nami</option>
          <option value="eternl">Eternl</option>
        </select>
        <Button
            style={{ marginRight: "16px" }}
            variant="success"
            size="sm"
            onClick={async e => {
                console.log("Connecting")
                await contractEndpoints.connect()
                setIsConnected(true)
                console.log("Connected")
                // setCurrentContractState(contractEndpoints.reload())
              }
            }
        >
          Connect Wallet
        </Button>
      </Nav.Link>
    </Navbar.Collapse>
  )
}

// Reload component that reloads the contract state
const Reload = () => {
  return (
    <>
    </>
  )
}

// Component that displays the form for starting a new Escrow.
const Start = ({show, handleShow, setShowStartModal, contractEndpoints}) => {

  const handleClose = e => {
    setShowStartModal(false)
    e.preventDefault()
    const formData = new FormData(e.target),
          recAddr = formData.get("recAddr") as string,
          sendAsset = formData.get("sendAsset") as string,
          sendAmount = parseInt(formData.get("sendAmount") as string),
          recAsset = formData.get("recAsset") as string,
          recAmount = parseInt(formData.get("recAmount") as string)

    const sp = mkStartParams(recAddr, sendAsset, sendAmount, recAsset, recAmount)
    console.log(sp)
    contractEndpoints.start()

  }
    return (
    <>
    <Modal show={show}>
        <Modal.Header closeButton>
          <Modal.Title>Start new Escrow</Modal.Title>
        </Modal.Header>
        <Modal.Body>
          <Form onSubmit={handleClose}>
            <Form.Group className="mb-3" controlId="startForm">
              <Form.Label>Receiver Address</Form.Label>
              <Form.Control
                name="recAddr"
                placeholder="Address"
                autoFocus
              />
              <br></br>
              <Row>
                <Col>
                    <Form.Label>Send AssetClass</Form.Label>
                    <Form.Control
                        name="sendAsset"
                        type="text"
                        placeholder="Asset"
                    />
                </Col>
                <Col>
                    <Form.Label>Send Amount</Form.Label>
                    <Form.Control
                        name="sendAmount"
                        type="number"
                        placeholder="Amount"
                    />
                </Col>
              </Row>
              <br></br>
              <Row>
                <Col>
                    <Form.Label>Receive AssetClass</Form.Label>
                    <Form.Control
                        name="recAsset"
                        placeholder="Asset"
                    />
                </Col>
                <Col>
                    <Form.Label>Receive Amount</Form.Label>
                    <Form.Control
                        name="recAmount"
                        type="number"
                        placeholder="Amount"
                    />
                </Col>
              </Row>
              <Button variant="primary" type="submit">
              Submit
              </Button>
            </Form.Group>
          </Form>
        </Modal.Body>
        <Modal.Footer
          style={{
            display: "flex",
            justifyContent: "center",
          }}
        >
          <Button variant="primary" onClick={handleClose}>
            Start
          </Button>
        </Modal.Footer>
      </Modal>
    </>
  )
}

// Component that displays the form for canceling started escrows.
const Cancel = () => {
  return (
    <>
    </>
  )
}

// Component that handles the resolving of started escrows.
const Resolve = () => {
  return (
    <>
    </>
  )
}

// Component that displays the started escrows in a table.
const ContractInformation = ( { currentContractState }  ) => {
  return (
    <div
      style={{
        textAlign:"center"
      }}
    >
    <Table striped bordered hover className="align-middle">
      <thead>
        <tr>
          <th>Sender Address</th>
          <th>Send Amount</th>
          <th>Send Asset</th>
          <th>Receive Amount</th>
          <th>Receive Asset</th>
          <th>Resolve</th>
        </tr>
      </thead>
      <tbody>
        {currentContractState.map((elem: UtxoInfo, index) => (
          <tr key={index}>
            <td> {elem.SenderAddr} </td>
            <td> {elem.SendAmount} </td>
            <td> {elem.SendAsset} </td>
            <td> {elem.ReceiveAmount} </td>
            <td> {elem.ReceiveAsset} </td>
            <td >
                <Button
                    style={{ marginRight: "16px" }}
                    variant="success"
                    size="sm"
                    onClick={async e => {
                        console.log("Resolving")
                        console.log(elem)
                    }
                  }
                > Resolve
                </Button>
            </td>
          </tr>
        ))}
      </tbody>
    </Table>
    </div>

  )
}

export default UserUI
