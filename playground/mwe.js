
import * as THREE from 'three';

const width = window.innerWidth, height = window.innerHeight;

// scene
const scene = new THREE.Scene();

// mesh
const geo1 = new THREE.SphereGeometry();
const mat1 = new THREE.MeshLambertMaterial();
const mesh1 = new THREE.Mesh(geo1, mat1);
scene.add( mesh1 );

// light
const pointLight = new THREE.PointLight();
pointLight.intensity = 200;
pointLight.position.set(8, 8, 8);
scene.add(pointLight);

// camera
const camera = new THREE.PerspectiveCamera( 70, width / height, 0.1, 100 );
camera.position.z = 6;

// renderer
const renderer = new THREE.WebGLRenderer( { antialias: true } );
renderer.setSize( width, height );
document.body.appendChild( renderer.domElement );

renderer.render( scene, camera );

